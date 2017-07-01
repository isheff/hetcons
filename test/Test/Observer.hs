{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Observer (observer_tests) where

import Hetcons.Contains_Value (extract_1a, extract_value)
import Hetcons.Hetcons_Exception (Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                                                   ,Hetcons_Exception_Unparsable_Hashable_Message
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                                                   ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Invalid_Signed_Hash))

import Hetcons.Hetcons_State
    ( Hetcons_State
    , Participant_State_Var
    , Participant_State
    , Observer_State_Var
    , Observer_State
    , default_State
    , conflicting_state
    , new_State
    , start_State
    , modify
    , read
    , modify_and_read
    , state_by_observers
    )
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Observer (Observer, new_observer, basic_observer_server)
import Hetcons.Participant (Participant, new_participant, basic_participant_server, current_nanoseconds )
import Hetcons.Receive_Message
  (Hetcons_Transaction
    ,run_Hetcons_Transaction_IO
    ,get_state
    ,put_state
    ,update_state
    ,get_my_crypto_id
    ,get_my_private_key
    ,with_errors
  ,Add_Sent
    ,add_sent
  ,Receivable
    ,receive
  ,Sendable
    ,send)
import Hetcons.Send_Message_IO (Address_Book, default_Address_Book, send_Message_IO)
import Hetcons.Signed_Message (Verified
                                 ,signed
                                 ,original
                              ,sign
                              ,verify
                              ,Recursive_1a
                              ,recursive_1a_filled_in
                              ,Recursive_1b
                              ,Recursive_2a (Recursive_2a)
                              ,Recursive_2b (Recursive_2b)
                              ,Recursive_Proof_of_Consensus
                              ,recursive_1b_proposal
                              ,recursive_1b_conflicting_phase2as
                              ,Parsable
                              ,non_recursive)


import qualified Hetcons_Participant_Client as Client (proposal_1a, phase_1b)
import Hetcons_Consts(sUPPORTED_HASH_SHA2_DESCRIPTOR
                     ,sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR)

import qualified Hetcons_Observer as Observer (process)
import qualified Hetcons_Observer_Iface as Observer (ping, phase_2b)
import Hetcons_Observer_Iface (Hetcons_Observer_Iface)
import Hetcons_Participant (process)
import Hetcons_Participant_Iface (Hetcons_Participant_Iface
                                   ,ping
                                   ,proposal_1a
                                   ,phase_1b)
import Hetcons_Types (Signed_Message
                        ,signed_Hash_signature
                        ,signed_Message_signature
                     ,default_No_Supported_Hash_Sha2_Descriptor_Provided
                     ,crypto_ID_public_crypto_key
                     ,default_Public_Crypto_Key
                     ,Crypto_ID
                       ,default_Crypto_ID
                     ,public_Crypto_Key_public_crypto_key_x509
                     ,signed_Message_payload
                     ,default_Descriptor_Does_Not_Match_Hash_Sha2
                     ,Proposal_1a
                        ,proposal_1a_value
                        ,proposal_1a_timestamp
                        ,default_Proposal_1a
                        ,proposal_1a_observers
                     ,Observers
                        ,default_Observers
                        ,observers_observer_quorums
                        ,observers_observer_graph
                     ,Value
                        ,value_value_payload
                        ,value_slot
                        ,default_Value
                     ,Phase_1b
                        ,phase_1b_proposal
                        ,phase_1b_conflicting_phase2as
                        ,default_Phase_1b
                     ,Phase_2a
                        ,phase_2a_phase_1bs
                        ,default_Phase_2a
                     ,Phase_2b
                        ,phase_2b_phase_1bs
                        ,default_Phase_2b
                     ,participant_ID_address
                     ,address_host_address
                     ,host_Address_dns_name
                     ,address_port_number
                     ,participant_ID_crypto_id
                     ,Participant_ID
                       ,default_Participant_ID
                     ,default_Address
                     ,default_Host_Address
                     ,Proof_of_Consensus
                       ,proof_of_Consensus_phase_2bs
                       ,default_Proof_of_Consensus
                     ,Observer_Trust_Constraint
                       ,observer_Trust_Constraint_observer_1
                       ,observer_Trust_Constraint_observer_2
                       ,observer_Trust_Constraint_safe
                       ,observer_Trust_Constraint_live
                       ,default_Observer_Trust_Constraint
                     ,Timestamp
                     )

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import Control.Exception (SomeException,
                          throw,
                          catch)
import           Control.Monad (join)
import           Control.Monad.Except   (runExceptT)
import Control.Monad.Trans.Except (except)
import Crypto.Random (getSystemDRG, DRG, withDRG)
import qualified Data.ByteString.Lazy as ByteString (readFile, concat, take, drop, singleton, index)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Combinators (isLeft, isRight)
import Data.Either.Combinators (mapRight)
import           Data.HashMap.Lazy           (empty)
import           Data.HashSet           (fromList,toList)
import Data.List (head)
import           Data.Serialize         (Serialize
                                           ,decodeLazy)
import Test.HUnit (Test(TestList,
                        TestLabel,
                        TestCase)
                  ,assertEqual
                  ,assertBool)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.HashMap.Strict (singleton)
import           Data.Text.Lazy         (pack)
import Thrift.Server (runBasicServer)
import GHC.IO.Handle (Handle)
import Network (PortID(PortNumber))
import Network.Socket (HostName)
import Thrift.Protocol (Protocol)
import Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import Thrift.Transport.Handle (hOpen)

doubleGen :: (DRG g) => g -> (g,g)
doubleGen g = withDRG g (return g)

listGen :: (DRG g) => g -> [g]
listGen g = g:(listGen (snd (withDRG g (return ()))))


sample_payload :: Integer
sample_payload = 1337

sample_message :: IO (Either Hetcons_Exception Signed_Message)
sample_message = sample_sign sample_payload


sample_sign :: (Serialize a) => a -> IO (Either Hetcons_Exception Signed_Message)
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
   proposal_1a_value = default_Value {
                          value_value_payload = ByteString.singleton 42
                         ,value_slot = 6}
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
  proposal_1a = on_proposal_1a
  phase_1b = on_phase_1b

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

launch_dummy_observer :: (Integral a) => a -> IO (a, Timestamp, Address_Book, ByteString)
launch_dummy_observer port = do
  { now <- current_nanoseconds
  ; receipt_2b <- newEmptyMVar
  ; address_book <- default_Address_Book
  ; cert <- ByteString.readFile "test/cert.pem"
  ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert (fromIntegral port)]
  ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
  ; (Right signed_1b) <- sample_sign $ default_Phase_1b { phase_1b_proposal = signed_1a }
  ; let (Right (v1b :: (Verified Recursive_1b))) = verify signed_1b
  ; (Right signed_2b) <- sample_sign $ default_Phase_2b { phase_2b_phase_1bs = fromList [signed_1b]}
  ; let (Right (v2b :: (Verified Recursive_2b))) = verify signed_2b
  ; dummy_observer <- dummy_observer_server port (Dummy_Observer { dummy_observer_on_ping = return ()
                                                                 , dummy_observer_on_phase_2b = putMVar receipt_2b})
  ; send_Message_IO address_book v2b
  ; takeMVar receipt_2b >>= assertEqual "received 2b is not sent 2b" signed_2b
  ; return (port, now, address_book, cert)
  }

launch_observer :: (Integral a) => a -> IO (a, Timestamp, Address_Book, ByteString, ByteString)
launch_observer port = do
  { (used_port, now, address_book, cert) <- launch_dummy_observer port
  ; let new_port = used_port + 1
  ; private <- ByteString.readFile "test/key.pem"
  ; observer <- (basic_observer_server
                  (default_Crypto_ID {
                    crypto_ID_public_crypto_key =
                      Just (default_Public_Crypto_Key {
                              public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                  private
                  $ fromIntegral new_port)
  ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert (fromIntegral new_port)]
  ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
  ; (Right signed_1b) <- sample_sign $ default_Phase_1b { phase_1b_proposal = signed_1a }
  ; let (Right (v1b :: (Verified Recursive_1b))) = verify signed_1b
  ; (Right signed_2b) <- sample_sign $ default_Phase_2b { phase_2b_phase_1bs = fromList [signed_1b]}
  ; let (Right (v2b :: (Verified Recursive_2b))) = verify signed_2b
  ; send_Message_IO address_book v2b
  ; assertBool "have launched an observer" True
  ; return (new_port, now, address_book, cert, private)
  }


observer_tests = TestList [

   TestLabel "Verify we can launch at least a dummy observer" (
     TestCase ( launch_dummy_observer 86000 >> return ()))

  ,TestLabel "Verify we can launch at a basic observer" (
     TestCase ( launch_observer 86001 >> return ()))
  ]

