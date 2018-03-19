{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Witness (witness_tests) where

import Hetcons.Hetcons_Exception ( Hetcons_Exception )
import Hetcons.Hetcons_State ( Participant_State_Var, start_State )
import Hetcons.Participant
    ( Participant, new_participant, current_nanoseconds, basic_participant_server, participant_server )
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
import Hetcons.Value ( Value, value_valid, value_conflicts, garbage_collect, extract_1a, extract_value )
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
import Control.Monad.Logger (runStdoutLoggingT)
import Crypto.Random ( getSystemDRG, DRG, withDRG )
import qualified Data.ByteString.Lazy as ByteString
    ( readFile, concat, take, drop, singleton, index, empty )
import Data.ByteString.Lazy ( ByteString)
import Data.HashSet ( fromList )
import Test.HUnit
    ( Test(TestList, TestLabel, TestCase), assertEqual, assertBool )
import qualified Data.HashMap.Strict as HashMap ( fromList )
import Data.Serialize ( Serialize, encodeLazy, decodeLazy )
import Data.Text.Lazy ( pack )
import Thrift.Server ( runBasicServer )
import GHC.IO.Handle ( Handle )
import Network ( PortID(PortNumber) )
import Network.Socket ( HostName )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Handle ( hOpen )



instance Value Int where
  value_valid witness v =
    case decodeLazy witness of
      (Left  _) -> return False
      (Right x) -> return ((x * x) == v)

  value_conflicts _ = False

  garbage_collect = id




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



witness_tests = TestList [


   TestLabel "Correctly finds Error while Verifying a 1a sent over the wire" (
     TestCase ( do
       { now <- current_nanoseconds
       ; cert1 <- ByteString.readFile "test/cert1.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; (participant :: (Participant Int)) <- new_participant runStdoutLoggingT 
                                                               (default_Crypto_ID {
                                                                  crypto_ID_public_crypto_key =
                                                                    Just (default_Public_Crypto_Key {
                                                                            public_Crypto_Key_public_crypto_key_x509 = Just cert1})}) private1
       ; participant_thread <- participant_server participant 87810
       ; address_book <- default_Address_Book
       ; (Right signed_1a) <- sample_sign
          (default_Proposal_1a {
             proposal_1a_value = ((encodeLazy (4 :: Int)) :: ByteString)
            ,proposal_1a_timestamp = now
            ,proposal_1a_observers = Just default_Observers {
                                           observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert1 87810, fromList [fromList [sample_id cert1 87810]])]}})
       ; let (Right (v1a :: (Verified (Recursive_1a Int)))) = verify signed_1a
       ; (handle :: Handle) <- hOpen (("localhost" :: HostName), PortNumber 87810)
       ; let client = (BinaryProtocol handle, BinaryProtocol handle)
       ; (catch (Client.proposal_1a client signed_1a ((encodeLazy (3 :: Int)) :: ByteString) >> assertBool "Exception should have been thrown" False)
                (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) True))
       }))

  ,TestLabel "Correctly does not find Error while Verifying a 1a sent over the wire" (
     TestCase ( do
       { now <- current_nanoseconds
       ; receipt_1b <- newEmptyMVar
       ; cert1 <- ByteString.readFile "test/cert1.pem"
       ; cert2 <- ByteString.readFile "test/cert2.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; (participant :: (Participant Int)) <- new_participant runStdoutLoggingT
                                                               (default_Crypto_ID {
                                                                  crypto_ID_public_crypto_key =
                                                                    Just (default_Public_Crypto_Key {
                                                                            public_Crypto_Key_public_crypto_key_x509 = Just cert1})}) private1
       ; dummy_thread <- dummy_participant_server 87812 (Dummy_Participant { on_ping =  return ()
                                                                           , on_proposal_1a = \_ -> return ()
                                                                           , on_phase_1b = putMVar receipt_1b})
       ; participant_thread <- participant_server participant 87811
       ; address_book <- default_Address_Book
       ; (Right signed_1a) <- sample_sign
          (default_Proposal_1a {
             proposal_1a_value = ((encodeLazy (4 :: Int)) :: ByteString)
            ,proposal_1a_timestamp = now
            ,proposal_1a_observers = Just default_Observers {
                observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert1 87811, fromList [fromList [sample_id cert1 87811, sample_id cert2 87812]])]}})
       ; let (Right (v1a :: (Verified (Recursive_1a Int)))) = verify signed_1a
       ; (handle :: Handle) <- hOpen (("localhost" :: HostName), PortNumber 87811)
       ; let client = (BinaryProtocol handle, BinaryProtocol handle)
       ; forkIO (catch (Client.proposal_1a client signed_1a ((encodeLazy (2 :: Int)) :: ByteString) >> assertBool "Exception should not have been thrown" True)
                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught when it should not have been: " ++ (show exception)) False))
       ; takeMVar receipt_1b
       ; assertBool "we should have made it to this point" True
       }))
  ]

