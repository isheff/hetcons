{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Observer (observer_tests) where

import Hetcons.Compact_Server (runCompactServer)
import Hetcons.Hetcons_Exception ( Hetcons_Exception )
import Hetcons.Instances_Proof_of_Consensus ( observers_proven )
import Hetcons.Observer (Observer, basic_observer_server )
import Hetcons.Participant ( current_nanoseconds )
import Hetcons.Send_Message_IO
    ( Address_Book
     ,default_Address_Book
     ,send_Message_IO
     ,domain_name )
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,Recursive_1b
     ,Recursive_1a
     ,Verified
     ,Recursive_2b
     ,Recursive_Proof_of_Consensus
     ,Monad_Verify(verify)
     ,sign )
import Test.Util ()

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
     ,Signed_Message
     ,Phase_1b(phase_1b_proposal)
              ,default_Phase_1b
     ,Phase_2b(phase_2b_phase_1bs)
              ,default_Phase_2b
     ,Host_Address(host_Address_dns_name)
                  ,default_Host_Address
     ,Timestamp
     ,Address(address_port_number, address_host_address)
             ,default_Address
    )

import Control.Concurrent ( forkIO, ThreadId )
import Control.Concurrent.MVar ( putMVar, takeMVar, newEmptyMVar )
import Crypto.Random ( getSystemDRG, DRG, withDRG )
import qualified Data.ByteString.Lazy as ByteString
    ( singleton, readFile, empty )
import Data.ByteString.Lazy ( ByteString )
import Data.HashSet ( fromList )
import Test.HUnit
    ( Test(TestList, TestLabel, TestCase), assertEqual, assertBool )
import qualified Data.HashMap.Strict as HashMap ( fromList )
import Data.HashMap.Strict ()
import Data.Text.Lazy ( pack )

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
dummy_participant_server port dummy = forkIO $ runCompactServer dummy process (fromIntegral port)


data Dummy_Observer = Dummy_Observer {
  dummy_observer_on_ping :: IO ()
 ,dummy_observer_on_phase_2b :: Signed_Message -> IO ()
}
instance Hetcons_Observer_Iface Dummy_Observer where
  ping = dummy_observer_on_ping
  phase_2b = dummy_observer_on_phase_2b

dummy_observer_server :: (Integral a) => a -> Dummy_Observer -> IO ThreadId
dummy_observer_server port dummy = forkIO $ runCompactServer dummy Observer.process (fromIntegral port)

launch_dummy_observer :: (Integral a) => a -> IO (a, Timestamp, Address_Book, ByteString)
launch_dummy_observer port = do
  { now <- current_nanoseconds
  ; receipt_2b <- newEmptyMVar
  ; address_book <- default_Address_Book
  ; cert <- ByteString.readFile "test/cert.pem"
  ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert (fromIntegral port)]
  ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
  ; (Right signed_1b) <- sample_sign $ default_Phase_1b { phase_1b_proposal = signed_1a }
  ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b
  ; (Right signed_2b) <- sample_sign $ default_Phase_2b { phase_2b_phase_1bs = fromList [signed_1b]}
  ; let (Right (v2b :: (Verified (Recursive_2b Slot_Value)))) = verify signed_2b
  ; dummy_observer <- dummy_observer_server port (Dummy_Observer { dummy_observer_on_ping = return ()
                                                                 , dummy_observer_on_phase_2b = putMVar receipt_2b})
  ; send_Message_IO address_book ByteString.empty v2b
  ; takeMVar receipt_2b >>= assertEqual "received 2b is not sent 2b" signed_2b
  ; return (port, now, address_book, cert)
  }

launch_observer :: (Integral a) => a -> IO (a, Timestamp, Address_Book, ByteString, ByteString)
launch_observer port = do
  { (used_port, now, address_book, cert) <- launch_dummy_observer port
  ; let new_port = used_port + 1
  ; private <- ByteString.readFile "test/key.pem"
  ; proof_receipt <- newEmptyMVar
  ; observer <- (basic_observer_server
                  (default_Crypto_ID {
                    crypto_ID_public_crypto_key =
                      Just (default_Public_Crypto_Key {
                              public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                  private
                  (fromIntegral new_port)
                  (putMVar proof_receipt :: ((Verified (Recursive_Proof_of_Consensus Slot_Value)) -> IO ())))
  ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert (fromIntegral new_port)]
  ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
  ; (Right signed_1b) <- sample_sign $ default_Phase_1b { phase_1b_proposal = signed_1a }
  ; let (Right (v1b :: (Verified (Recursive_1b Slot_Value)))) = verify signed_1b
  ; (Right signed_2b) <- sample_sign $ default_Phase_2b { phase_2b_phase_1bs = fromList [signed_1b]}
  ; let (Right (v2b :: (Verified (Recursive_2b Slot_Value)))) = verify signed_2b
  ; send_Message_IO address_book ByteString.empty v2b
  ; assertBool "have launched an observer" True
  ; received_proof <- takeMVar proof_receipt
  ; assertEqual "incorrect observers proven" ("localhost:"++(show $ fromIntegral new_port)++",") $
      foldr (\n x -> x ++ (domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n) ++",") "" $ observers_proven received_proof
  ; return (new_port, now, address_book, cert, private)
  }


observer_tests = TestList [

   TestLabel "Verify we can launch at least a dummy observer" (
     TestCase ( launch_dummy_observer 86000 >> return ()))

  ,TestLabel "Verify we can launch at a basic observer" (
     TestCase ( launch_observer 86001 >> return ()))
  ]

