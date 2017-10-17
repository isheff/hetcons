{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Participant (participant_tests) where

import Hetcons.Hetcons_Exception ( Hetcons_Exception )
import Hetcons.Hetcons_State ( Participant_State_Var, start_State )
import Hetcons.Participant
    ( Participant, new_participant, current_nanoseconds, basic_participant_server )
import Hetcons.Receive_Message
    ( Hetcons_Server(hetcons_Server_state_var
                    ,hetcons_Server_address_book)
     ,Receivable(receive)
     ,run_Hetcons_Transaction_IO )
import Hetcons.Send_Message_IO
    ( send_Message_IO, default_Address_Book )
import Hetcons.Signed_Message
    ( Encodable
        ,encode
     ,Recursive_1b(recursive_1b_conflicting_phase2as)
     ,Recursive_1a
     ,Verified
     ,Recursive_2b
     ,Monad_Verify(verify)
     ,sign
     ,original )
import Hetcons.Value ( extract_1a, extract_value )
import Test.Util ()

import qualified Hetcons_Participant_Client as Client
    ( proposal_1a )
import Charlotte_Consts ( sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR )
import qualified Hetcons_Observer as Observer ( process )
import qualified Hetcons_Observer_Iface as Observer
    ( ping, phase_2b )
import Hetcons_Observer_Iface ( Hetcons_Observer_Iface )
import Hetcons_Participant ( process )
import Hetcons_Participant_Iface
    ( Hetcons_Participant_Iface, ping, proposal_1a, phase_1b )
import Charlotte_Types
    ( Participant_ID(participant_ID_crypto_id, participant_ID_address)
                    ,default_Participant_ID
     ,Slot_Value(slot_Value_slot, slot_Value_value_payload)
           ,default_Slot_Value
     ,Observers(observers_observer_quorums)
               ,default_Observers
     ,Proposal_1a(proposal_1a_observers, proposal_1a_timestamp
                 ,proposal_1a_value)
                 ,default_Proposal_1a
     ,Public_Crypto_Key(public_Crypto_Key_public_crypto_key_x509)
                       ,default_Public_Crypto_Key
     ,Crypto_ID(crypto_ID_public_crypto_key)
               ,default_Crypto_ID
     ,Signed_Message(signed_Message_signature)
     ,Phase_1b(phase_1b_proposal)
              ,default_Phase_1b
     ,Phase_2b(phase_2b_phase_1bs)
              ,default_Phase_2b
     ,Host_Address(host_Address_dns_name)
                  ,default_Host_Address
     ,Timestamp
     ,Address(address_port_number, address_host_address)
             ,default_Address
     ,signed_Hash_signature
    )

import Control.Concurrent ( forkIO, ThreadId )
import Control.Concurrent.MVar ( putMVar, takeMVar, newEmptyMVar )
import Control.Exception ( SomeException, catch )
import Crypto.Random ( getSystemDRG, DRG, withDRG )
import qualified Data.ByteString.Lazy as ByteString
    ( readFile, concat, take, drop, singleton, index, empty )
import Data.HashSet ( fromList )
import Test.HUnit
    ( Test(TestList, TestLabel, TestCase), assertEqual, assertBool )
import qualified Data.HashMap.Strict as HashMap ( fromList )
import Data.Text.Lazy ( pack )
import Thrift.Server ( runBasicServer )
import GHC.IO.Handle ( Handle )
import Network ( PortID(PortNumber) )
import Network.Socket ( HostName )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Handle ( hOpen )

doubleGen :: (DRG g) => g -> (g,g)
doubleGen g = withDRG g (return g)

listGen :: (DRG g) => g -> [g]
listGen g = g:(listGen (snd (withDRG g (return ()))))


sample_payload :: Integer
sample_payload = 1337

sample_message :: IO (Either Hetcons_Exception Signed_Message)
sample_message = sample_sign sample_payload


sample_sign :: (Encodable a) => a -> IO (Either Hetcons_Exception Signed_Message)
sample_sign payload =
  do { gen <- getSystemDRG
     ; cert <- ByteString.readFile "test/cert.pem"
     ; private <- ByteString.readFile "test/key.pem"
     ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert})}
     ; return $ sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen payload}

sample_id cert port =
  default_Participant_ID  {
    participant_ID_address =
      default_Address  {
        address_host_address =
          default_Host_Address  {
            host_Address_dns_name = Just $ pack "localhost"}
       ,address_port_number = port}
   ,participant_ID_crypto_id =
      default_Crypto_ID {
        crypto_ID_public_crypto_key =
          Just (default_Public_Crypto_Key {
                  public_Crypto_Key_public_crypto_key_x509 = Just cert})}}

-- sample_1a :: Proposal_1a
sample_1a now recipients = default_Proposal_1a {
   proposal_1a_value = encode default_Slot_Value {
                          slot_Value_value_payload = ByteString.singleton 42
                         ,slot_Value_slot = 6}
  ,proposal_1a_timestamp = now
  ,proposal_1a_observers = Just default_Observers {
     observers_observer_quorums = Just $ HashMap.fromList
      [(r, fromList [fromList recipients]) | r <- recipients] }}



deStupidify :: (Monad m) => Either a (m b) -> (m (Either a b))
deStupidify (Left  x) = return (Left x)
deStupidify (Right x) = do { y <- x
                           ; return (Right y)}


data Dummy_Participant = Dummy_Participant {
  on_ping :: IO ()
 ,on_proposal_1a :: Signed_Message -> IO ()
 ,on_phase_1b :: Signed_Message -> IO ()
}
instance Hetcons_Participant_Iface Dummy_Participant where
  ping = on_ping
  proposal_1a v x _ = on_proposal_1a v x
  phase_1b v x _ = on_phase_1b v x

dummy_participant_server :: (Integral a) => a -> Dummy_Participant -> IO ThreadId
dummy_participant_server port dummy = forkIO $ runBasicServer dummy process (fromIntegral port)


data Dummy_Observer = Dummy_Observer {
  dummy_observer_on_ping :: IO ()
 ,dummy_observer_on_phase_2b :: Signed_Message -> IO ()
}
instance Hetcons_Observer_Iface Dummy_Observer where
  ping = dummy_observer_on_ping
  phase_2b = dummy_observer_on_phase_2b

dummy_observer_server :: (Integral a) => a -> Dummy_Observer -> IO ThreadId
dummy_observer_server port dummy = forkIO $ runBasicServer dummy Observer.process (fromIntegral port)



participant_tests = TestList [

   TestLabel "Verify we can launch at least a dummy participant" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_ping <- newEmptyMVar
       ; receipt_1a <- newEmptyMVar
       ; receipt_1b <- newEmptyMVar
       ; dummy_thread <- dummy_participant_server 87001 (Dummy_Participant { on_ping = putMVar receipt_ping True
                                                                           , on_proposal_1a = putMVar receipt_1a
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; address_book <- default_Address_Book
       ; cert <- ByteString.readFile "test/cert.pem"
       ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert 87001]
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; (Right signed_1b) <- sample_sign $ default_Phase_1b { phase_1b_proposal = signed_1a }
       ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b
       ; send_Message_IO address_book ByteString.empty v1a
       ; send_Message_IO address_book ByteString.empty v1b
       ; r1a <- takeMVar receipt_1a
       ; r1b <- takeMVar receipt_1b
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a
       ; assertEqual "received 1b is not sent 1b" signed_1b r1b
       }))

  ,TestLabel "Verify we can launch at least 3 dummy participants" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_ping <- newEmptyMVar
       ; receipt_1a <- newEmptyMVar
       ; receipt_1b <- newEmptyMVar
       ; receipt_ping2 <- newEmptyMVar
       ; receipt_1a2 <- newEmptyMVar
       ; receipt_1b2 <- newEmptyMVar
       ; receipt_ping3 <- newEmptyMVar
       ; receipt_1a3 <- newEmptyMVar
       ; receipt_1b3 <- newEmptyMVar
       ; dummy_thread <- dummy_participant_server 87004 (Dummy_Participant { on_ping = putMVar receipt_ping True
                                                                           , on_proposal_1a = putMVar receipt_1a
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; dummy_thread2 <- dummy_participant_server 87002 (Dummy_Participant { on_ping = putMVar receipt_ping2 True
                                                                           , on_proposal_1a = putMVar receipt_1a2
                                                                           , on_phase_1b = putMVar receipt_1b2})
       ; dummy_thread3 <- dummy_participant_server 87003 (Dummy_Participant { on_ping = putMVar receipt_ping3 True
                                                                           , on_proposal_1a = putMVar receipt_1a3
                                                                           , on_phase_1b = putMVar receipt_1b3})
       ; address_book <- default_Address_Book
       ; cert <- ByteString.readFile "test/cert.pem"
       ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert 87004,sample_id cert 87002,sample_id cert 87003]
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; (Right signed_1b) <- sample_sign $ default_Phase_1b { phase_1b_proposal = signed_1a }
       ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b
       ; send_Message_IO address_book ByteString.empty v1a
       ; send_Message_IO address_book ByteString.empty v1b
       ; r1a <- takeMVar receipt_1a
       ; r1b <- takeMVar receipt_1b
       ; r1a2 <- takeMVar receipt_1a2
       ; r1b2 <- takeMVar receipt_1b2
       ; r1a3 <- takeMVar receipt_1a3
       ; r1b3 <- takeMVar receipt_1b3
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a
       ; assertEqual "received 1b is not sent 1b" signed_1b r1b
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a2
       ; assertEqual "received 1b is not sent 1b" signed_1b r1b2
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a3
       ; assertEqual "received 1b is not sent 1b" signed_1b r1b3
       }))

  ,TestLabel "Correctly finds Error while Verifying a 1a sent over the wire" (
     TestCase ( do
       { now <- current_nanoseconds
       ; cert1 <- ByteString.readFile "test/cert1.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; participant_thread <- (basic_participant_server
                                 (default_Crypto_ID {
                                  crypto_ID_public_crypto_key =
                                    Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert1})})
                                 private1
                                 87010)
       ; address_book <- default_Address_Book
       ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert1 87010]
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; let borked_message =(signed_1a{
                  signed_Message_signature =
                    ((signed_Message_signature signed_1a) {
                        signed_Hash_signature =
                          let s = (signed_Hash_signature $ signed_Message_signature signed_1a)
                           in ByteString.concat [ByteString.take 42 s
                                                ,ByteString.singleton ( 1 + (ByteString.index s 42))
                                                ,ByteString.drop 43 s]
                      })})
       ; (handle :: Handle) <- hOpen (("localhost" :: HostName), PortNumber 87010)
       ; let client = (BinaryProtocol handle, BinaryProtocol handle)
       ; (catch (Client.proposal_1a client borked_message ByteString.empty >> assertBool "Exception should have been thrown" False)
                (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) True))
       }))

  ,TestLabel "Run a Participant transaction" (
     TestCase ( do
       { now <- current_nanoseconds
       ; cert1 <- ByteString.readFile "test/cert.pem"
       ; cert2 <- ByteString.readFile "test/cert2.pem"
       ; private1 <- ByteString.readFile "test/key.pem"
       ; address_book <- default_Address_Book
       ; (sv :: Participant_State_Var Slot_Value) <- start_State
       ; let cid = (default_Crypto_ID {
                     crypto_ID_public_crypto_key =
                       Just (default_Public_Crypto_Key {
                               public_Crypto_Key_public_crypto_key_x509 = Just cert1})})
       ; dummy_thread <- dummy_participant_server 87020 (Dummy_Participant { on_ping = return ()
                                                                           , on_proposal_1a = \_-> return ()
                                                                           , on_phase_1b = \_ -> return () })
       ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert1 87020, sample_id cert2 87020]
       ; let (Right (verified :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; (participant :: Participant Slot_Value) <- new_participant cid private1
       ; catch (catch (run_Hetcons_Transaction_IO (participant{ hetcons_Server_address_book = address_book
                                                              , hetcons_Server_state_var = sv})
                                                  (\_ -> return ())
                                                  ByteString.empty
                                                  $ receive verified)
                (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
         (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) False))
       }))

  ,TestLabel "Participant produces 1b given 1a" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_ping <- newEmptyMVar
       ; receipt_1a <- newEmptyMVar
       ; receipt_1b <- newEmptyMVar
       ; receipt_ping2 <- newEmptyMVar
       ; receipt_1a2 <- newEmptyMVar
       ; receipt_1b2 <- newEmptyMVar
       ; cert <- ByteString.readFile "test/cert.pem"
       ; private <- ByteString.readFile "test/key.pem"
       ; cert2 <- ByteString.readFile "test/cert2.pem"
       ; cert3 <- ByteString.readFile "test/cert3.pem"
       ; participant_thread <- (basic_participant_server
                                 (default_Crypto_ID {
                                  crypto_ID_public_crypto_key =
                                    Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                                 private
                                 87007)
       ; dummy_thread <- dummy_participant_server 87005 (Dummy_Participant { on_ping = putMVar receipt_ping True
                                                                           , on_proposal_1a = putMVar receipt_1a
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; dummy_thread2 <- dummy_participant_server 87006 (Dummy_Participant { on_ping = putMVar receipt_ping2 True
                                                                           , on_proposal_1a = putMVar receipt_1a2
                                                                           , on_phase_1b = putMVar receipt_1b2})
       ; address_book <- default_Address_Book
       ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert2 87005,sample_id cert3 87006,sample_id cert 87007]
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1a)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; r1a <- takeMVar receipt_1a
       ; r1b <- takeMVar receipt_1b
       ; r1a2 <- takeMVar receipt_1a2
       ; r1b2 <- takeMVar receipt_1b2
       ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify r1b
       ; let (Right (v1b2 :: (Verified (Recursive_1b Slot_Value)))) = verify r1b2
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a2
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b2
       }))

  ,TestLabel "Participant ignores 1a if it has received greater timestamp 1a" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_ping <- newEmptyMVar
       ; receipt_1a <- newEmptyMVar
       ; receipt_1b <- newEmptyMVar
       ; receipt_ping2 <- newEmptyMVar
       ; receipt_1a2 <- newEmptyMVar
       ; receipt_1b2 <- newEmptyMVar
       ; cert <- ByteString.readFile "test/cert.pem"
       ; private <- ByteString.readFile "test/key.pem"
       ; cert2 <- ByteString.readFile "test/cert2.pem"
       ; cert3 <- ByteString.readFile "test/cert3.pem"
       ; participant_thread <- (basic_participant_server
                                 (default_Crypto_ID {
                                  crypto_ID_public_crypto_key =
                                    Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                                 private
                                 87037)
       ; dummy_thread <- dummy_participant_server 87035 (Dummy_Participant { on_ping = putMVar receipt_ping True
                                                                           , on_proposal_1a = putMVar receipt_1a
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; dummy_thread2 <- dummy_participant_server 87036 (Dummy_Participant { on_ping = putMVar receipt_ping2 True
                                                                           , on_proposal_1a = putMVar receipt_1a2
                                                                           , on_phase_1b = putMVar receipt_1b2})
       ; address_book <- default_Address_Book
       ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert3 87035,sample_id cert2 87036,sample_id cert 87037]
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1a)
                                             (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                                      (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; r1a <- takeMVar receipt_1a
       ; r1b <- takeMVar receipt_1b
       ; r1a2 <- takeMVar receipt_1a2
       ; r1b2 <- takeMVar receipt_1b2
       ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify r1b
       ; let (Right (v1b2 :: (Verified (Recursive_1b Slot_Value)))) = verify r1b2
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b
       ; assertEqual "received 1a is not sent 1a" signed_1a r1a2
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b2
       ; (Right signed_1a2) <- sample_sign $ sample_1a (now - 1) [sample_id cert3 87035,sample_id cert2 87036,sample_id cert 87037]
       ; let (Right (v1a2 :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a2
       ; (catch (catch (send_Message_IO address_book ByteString.empty v1a2)
                       (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) False)))
       ; r1a12 <- takeMVar receipt_1a
       ; r1a22 <- takeMVar receipt_1a2
       ; assertEqual "received 1a is not sent 1a" signed_1a2 r1a12
       ; assertEqual "received 1a is not sent 1a" signed_1a2 r1a22
       ; (Right signed_1a3) <- sample_sign $ sample_1a (now + 1) [sample_id cert3 87035,sample_id cert2 87036,sample_id cert 87037]
       ; let (Right (v1a3 :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a3
       ; send_thread2 <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1a3)
                                              (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                                       (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; r1a13 <- takeMVar receipt_1a
       ; r1b13 <- takeMVar receipt_1b
       ; r1a23 <- takeMVar receipt_1a2
       ; r1b23 <- takeMVar receipt_1b2
       ; let (Right (v1b13 :: (Verified (Recursive_1b Slot_Value)))) = verify r1b13
       ; let (Right (v1b23 :: (Verified (Recursive_1b Slot_Value)))) = verify r1b23
       ; assertEqual "received 1a is not sent 1a" signed_1a3 r1a13
       ; assertEqual "received 1b is not sent 1b" v1a3 $ extract_1a v1b13
       ; assertEqual "received 1a is not sent 1a" signed_1a3 r1a23
       ; assertEqual "received 1b is not sent 1b" v1a3 $ extract_1a v1b23
       }))

  ,TestLabel "Participant produces 1b given 1b" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_1b <- newEmptyMVar
       ; receipt_1b2 <- newEmptyMVar
       ; cert <- ByteString.readFile "test/cert1.pem"
       ; private <- ByteString.readFile "test/key1.pem"
       ; participant_thread <- (basic_participant_server
                                 (default_Crypto_ID {
                                  crypto_ID_public_crypto_key =
                                    Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                                 private
                                 87047)
       ; dummy_thread <- dummy_participant_server 87045 (Dummy_Participant { on_ping = return ()
                                                                           , on_proposal_1a = \_ -> return ()
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; dummy_thread2 <- dummy_participant_server 87046 (Dummy_Participant { on_ping = return ()
                                                                            , on_proposal_1a = \_ -> return ()
                                                                            , on_phase_1b = putMVar receipt_1b2})
       ; address_book <- default_Address_Book
       ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert 87045,sample_id cert 87046,sample_id cert 87047]
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; (Right signed_1b) <- sample_sign (default_Phase_1b { phase_1b_proposal = signed_1a})
       ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1b)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; r1b <- takeMVar receipt_1b
       ; r1b2 <- takeMVar receipt_1b2
       ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify r1b
       ; let (Right (v1b2 :: (Verified (Recursive_1b Slot_Value)))) = verify r1b2
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b2
       }))

  ,TestLabel "Participant produces 2b given appropriate 1bs" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_2b <- newEmptyMVar
       ; receipt_1b <- newEmptyMVar
       ; cert <- ByteString.readFile "test/cert.pem"
       ; private <- ByteString.readFile "test/key.pem"
       ; cert1 <- ByteString.readFile "test/cert1.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; cert2 <- ByteString.readFile "test/cert2.pem"
       ; private2 <- ByteString.readFile "test/key2.pem"
       ; cert3 <- ByteString.readFile "test/cert3.pem"
       ; private3 <- ByteString.readFile "test/key3.pem"
       ; cert4 <- ByteString.readFile "test/cert4.pem"
       ; private4 <- ByteString.readFile "test/key4.pem"
       ; participant_thread <- (basic_participant_server
                                 (default_Crypto_ID {
                                  crypto_ID_public_crypto_key =
                                    Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert1})})
                                 private1
                                 87057)
       ; dummy_thread <- dummy_participant_server 87055 (Dummy_Participant { on_ping = return ()
                                                                           , on_proposal_1a = \_ -> return ()
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; dummy_thread2 <- dummy_participant_server 87056 (Dummy_Participant { on_ping = return ()
                                                                            , on_proposal_1a = \_ -> return ()
                                                                            , on_phase_1b = \_ -> return ()})
       ; dummy_observer <- dummy_observer_server 87058 (Dummy_Observer { dummy_observer_on_ping = return ()
                                                                       , dummy_observer_on_phase_2b = putMVar receipt_2b})
       ; address_book <- default_Address_Book
       ; let message_1a = default_Proposal_1a {
                            proposal_1a_value = encode default_Slot_Value {
                                                   slot_Value_value_payload = ByteString.singleton 42
                                                  ,slot_Value_slot = 6}
                           ,proposal_1a_timestamp = now
                           ,proposal_1a_observers = Just default_Observers {
                              observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert4 87058,
                                                                    fromList [fromList [sample_id cert1 87055,sample_id cert2 87056,sample_id cert3 87057]])]}}
       ; (Right signed_1a) <- sample_sign $ message_1a
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; gen2 <- getSystemDRG
       ; let crypto_id2 = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert2})}
       ; let (Right signed_1b2) = sign crypto_id2 private2 sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen2 (default_Phase_1b { phase_1b_proposal = signed_1a})
       ; gen3 <- getSystemDRG
       ; let crypto_id3 = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert3})}
       ; let (Right signed_1b3) = sign crypto_id3 private3 sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen3 (default_Phase_1b { phase_1b_proposal = signed_1a})
       ; let (Right (v1b2 :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b2
       ; let (Right (v1b3 :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b3
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1b2)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; send_thread2 <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1b3)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; r1b <- takeMVar receipt_1b
       ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify r1b
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b
       ; r1b2 <- takeMVar receipt_1b
       ; let (Right (v1b2 :: (Verified (Recursive_1b Slot_Value)))) = verify r1b2
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b2
       ; r1b3 <- takeMVar receipt_1b
       ; let (Right (v1b3 :: (Verified (Recursive_1b Slot_Value)))) = verify r1b3
       ; assertEqual "received 1b is not sent 1b" v1a $ extract_1a v1b3
       ; r2b <- takeMVar receipt_2b
       ; let (Right (v2b :: (Verified (Recursive_2b Slot_Value)))) = verify r2b
       ; assertEqual "received 2b is not correct" v1a $ extract_1a v2b
       }))


  ,TestLabel "After accepting a value, new 1bs feature previously accepted value" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_2b <- newEmptyMVar
       ; receipt_1b <- newEmptyMVar
       ; cert <- ByteString.readFile "test/cert.pem"
       ; private <- ByteString.readFile "test/key.pem"
       ; cert1 <- ByteString.readFile "test/cert1.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; cert2 <- ByteString.readFile "test/cert2.pem"
       ; private2 <- ByteString.readFile "test/key2.pem"
       ; cert3 <- ByteString.readFile "test/cert3.pem"
       ; private3 <- ByteString.readFile "test/key3.pem"
       ; cert4 <- ByteString.readFile "test/cert4.pem"
       ; private4 <- ByteString.readFile "test/key4.pem"
       ; participant_thread <- (basic_participant_server
                                 (default_Crypto_ID {
                                  crypto_ID_public_crypto_key =
                                    Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert1})})
                                 private1
                                 77097)
       ; dummy_thread <- dummy_participant_server 77095 (Dummy_Participant { on_ping = return ()
                                                                           , on_proposal_1a = \_ -> return ()
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; dummy_thread2 <- dummy_participant_server 77096 (Dummy_Participant { on_ping = return ()
                                                                            , on_proposal_1a = \_ -> return ()
                                                                            , on_phase_1b = \_ -> return ()})
       ; dummy_observer <- dummy_observer_server 77098 (Dummy_Observer { dummy_observer_on_ping = return ()
                                                                       , dummy_observer_on_phase_2b = putMVar receipt_2b})
       ; address_book <- default_Address_Book
       ; let original_value = encode default_Slot_Value { slot_Value_value_payload = ByteString.singleton 43
                                            , slot_Value_slot = 6}
       ; let message_1a = default_Proposal_1a {
                            proposal_1a_value = original_value
                           ,proposal_1a_timestamp = now
                           ,proposal_1a_observers = Just default_Observers {
                              observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert4 77098,
                                                                    fromList [fromList [sample_id cert1 77095,sample_id cert2 77096,sample_id cert3 77097]])]}}
       ; (Right signed_1a) <- sample_sign $ message_1a
       ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
       ; gen2 <- getSystemDRG
       ; let crypto_id2 = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert2})}
       ; let (Right signed_1b2) = sign crypto_id2 private2 sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen2 (default_Phase_1b { phase_1b_proposal = signed_1a})
       ; gen3 <- getSystemDRG
       ; let crypto_id3 = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert3})}
       ; let (Right signed_1b3) = sign crypto_id3 private3 sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen3 (default_Phase_1b { phase_1b_proposal = signed_1a})
       ; let (Right (v1b2 :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b2
       ; let (Right (v1b3 :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b3
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1b2)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; send_thread2 <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1b3)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; let receive_1b = do { r1b <- takeMVar receipt_1b
                             ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify r1b
                             ; assertEqual ("received 1b is not the original sent 1b\n"++(show $ recursive_1b_conflicting_phase2as $ original v1b)) ((extract_value v1a) :: Slot_Value) $ extract_value v1b}
       ; sequence $ take 5 $ repeat receive_1b
       ; r2b <- takeMVar receipt_2b
       ; let (Right (v2b :: (Verified (Recursive_2b Slot_Value)))) = verify r2b
       ; assertEqual "received 2b is not correct" v1a $ extract_1a v2b
       ; let message_1a2= default_Proposal_1a {
                            proposal_1a_value = encode default_Slot_Value {
                                                   slot_Value_value_payload = ByteString.singleton 42
                                                  ,slot_Value_slot = 6}
                           ,proposal_1a_timestamp = now + 1
                           ,proposal_1a_observers = Just default_Observers {
                              observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert4 77098,
                                                                    fromList [fromList [sample_id cert1 77095,sample_id cert2 77096,sample_id cert3 77097]])]}}
       ; (Right signed_1a2) <- sample_sign $ message_1a2
       ; let (Right (v1a2 :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a2
       ; send_thread3 <- forkIO (catch (catch (send_Message_IO address_book ByteString.empty v1a2)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; receive_1b
       }))

  ]

