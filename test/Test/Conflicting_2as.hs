{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Conflicting_2as (conflicting_2as_tests) where

import Hetcons.Contains_Value (extract_1a, extract_value, extract_observer_quorums)
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
import Hetcons.Instances_1b_2a (well_formed_2a)
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
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
                                ,recursive_1b_non_recursive
                                ,recursive_1b_proposal
                                ,recursive_1b_conflicting_phase2as
                              ,Recursive_2a (Recursive_2a)
                              ,Recursive_2b (Recursive_2b)
                              ,Recursive_Proof_of_Consensus
                              ,recursive_1b_proposal
                              ,recursive_1b_conflicting_phase2as
                              ,Parsable
                              ,non_recursive)
import Hetcons.Value (conflicts)


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
                     ,Phase_2a(Phase_2a)
                        ,phase_2a_phase_1bs
                        ,default_Phase_2a
                     ,Phase_2b(Phase_2b)
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
                     )

import Control.Concurrent (forkIO, ThreadId, threadDelay)
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
import qualified Data.HashSet as HashSet (map, filter)
import           Data.HashSet           (HashSet, fromList,toList, insert)
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



conflicting_2as_tests = TestList [

  TestLabel "finds correct conflicting 2as" (
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
       ; let original_value = default_Value { value_value_payload = ByteString.singleton 43
                                            , value_slot = 6}
       ; let message_1a = default_Proposal_1a {
                            proposal_1a_value = original_value
                           ,proposal_1a_timestamp = now
                           ,proposal_1a_observers = Just default_Observers {
                              observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert4 77098,
                                                                    fromList [fromList [sample_id cert1 77095,sample_id cert2 77096,sample_id cert3 77097]])]}}
       ; (Right signed_1a) <- sample_sign $ message_1a
       ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
       ; gen1 <- getSystemDRG
       ; let crypto_id1 = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert1})}
       ; let (Right signed_1b1) = sign crypto_id1 private1 sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen1 (default_Phase_1b { phase_1b_proposal = signed_1a})
       ; let (Right (v1b1 :: (Verified Recursive_1b))) = verify signed_1b1
       ; assertEqual "1a preserved" v1a $ extract_1a v1b1
       ; putStrLn "BEFORE NON_RECURSIVE"
       ; putStrLn $ show $ length $ show $ recursive_1b_non_recursive $ original v1b1
       ; putStrLn "AFTER NON_RECURSIVE"
       ; putStrLn "BEFORE PROPOSAL"
       ; putStrLn $ show $ length $ show $ recursive_1b_proposal $ original v1b1
       ; putStrLn "AFTER PROPOSAL"
       ; putStrLn "BEFORE CONFLICTING"
       ; putStrLn $ show $ length $ show $ recursive_1b_conflicting_phase2as $ original v1b1
       ; putStrLn "AFTER CONFLICTING"
       ; putStrLn "BEFORE SIGNED"
       ; putStrLn $ show $ length $ show $ signed v1b1
       ; putStrLn "AFTER SIGNED"
       ; putStrLn "BEFORE ORIGINAL"
       ; putStrLn $ show $ length $ show $ original v1b1
       ; putStrLn "AFTER ORIGINAL"
       ; putStrLn "BEFORE"
       ; putStrLn $ show $ length $ show v1b1
       ; putStrLn "AFTER"
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
       ; let (Right (v1b2 :: (Verified Recursive_1b))) = verify signed_1b2
       ; let (Right (v1b3 :: (Verified Recursive_1b))) = verify signed_1b3
       ; let message_1a2= default_Proposal_1a {
                            proposal_1a_value = default_Value {
                                                   value_value_payload = ByteString.singleton 42
                                                  ,value_slot = 6}
                           ,proposal_1a_timestamp = now + 1
                           ,proposal_1a_observers = Just default_Observers {
                              observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert4 77098,
                                                                    fromList [fromList [sample_id cert1 77095,sample_id cert2 77096,sample_id cert3 77097]])]}}
       ; (Right signed_1a2) <- sample_sign $ message_1a2
       ; let (Right (v1a2 :: (Verified Recursive_1a))) = verify signed_1a2
       ; let correct_2as = fromList [default_Phase_2a  { phase_2a_phase_1bs = fromList [signed_1b1, signed_1b2, signed_1b3]}]
       ; let existing_1bs = [v1b1, v1b2, v1b3]
       ; putStrLn "BEFORE EXISTING_1Bs"
       ; putStrLn $ show $ length $ show existing_1bs
       ; putStrLn "AFTER EXISTING_1BS"
       ; putStrLn "BEFORE CORRECT_2As"
       ; putStrLn $ show $ length $ show correct_2as
       ; putStrLn "AFTER CORRECT_2As"
       ; let conflicting_2as old_1bs new_message =do {
               let quorums_1bs = fromList $ filter (conflicts . (\x -> insert x $ fromList [ extract_value new_message]) . extract_value) $ -- TODO: non-pairwise conflicts
                                            filter (((extract_observer_quorums new_message) ==) . extract_observer_quorums) $ toList old_1bs -- same quorums
              ;putStrLn "BEFORE quorums_1bs"
              ;putStrLn $ show $ length $ show quorums_1bs
              ;putStrLn "AFTER quorums_1bs"
              ;let with_same_1a = HashSet.map (\x -> HashSet.filter (\y -> ((extract_1a x) == (extract_1a y))) quorums_1bs) quorums_1bs
              ;putStrLn "BEFORE with_same_1a"
              ;putStrLn $ show $ length $ show with_same_1a
              ;putStrLn "AFTER with_same_1a"
              ;let with_same_value = HashSet.map (\x -> HashSet.filter (\y -> ((extract_value x) == (extract_value y))) quorums_1bs) quorums_1bs
              ;putStrLn "BEFORE with_same_value"
              ;putStrLn $ show $ length $ show with_same_value
              ;putStrLn "AFTER with_same_value"
              ;let with_both = HashSet.map (\x -> HashSet.filter (\y -> (((extract_value x) == (extract_value y)) && (((extract_1a x) == (extract_1a y))))) quorums_1bs) quorums_1bs
              ;putStrLn "BEFORE with_both"
              ;putStrLn $ show $ length $ show with_both
              ;putStrLn "AFTER with_both"
              ;let potential_2as = HashSet.map Recursive_2a with_both
              ;putStrLn "BEFORE potential_2as"
              ;putStrLn $ show $ length $ show potential_2as
              ;putStrLn "AFTER potential_2as"
              ;return $ HashSet.map non_recursive $ HashSet.filter (\x -> case well_formed_2a x of
                                                                       Left _ -> False
                                                                       Right _ -> True) potential_2as}
       ; calculated_2as <- conflicting_2as (fromList existing_1bs) v1a2
       ; putStrLn "BEFORE CALCULATED"
       ; forkIO $ putStrLn $ show $ length $ show (calculated_2as :: (HashSet Phase_2a))
       ; threadDelay 10000000
       }))

  ]

