{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Consensus (consensus_tests) where

import Hetcons.Contains_Value ( Contains_Value(extract_value) )
import Hetcons.Hetcons_Exception ( Hetcons_Exception )
import Hetcons.Hetcons_State ( Participant_State_Var, start_State )
import Hetcons.Instances_Proof_of_Consensus ( observers_proven )
import Hetcons.Observer
    ( Observer(Observer, do_on_consensus, observer_hetcons_server)
        ,observer_server
        ,basic_observer_server )
import Hetcons.Participant
    ( participant_server
     ,new_participant
     ,current_nanoseconds
     ,basic_participant_server )
import Hetcons.Receive_Message
    ( Hetcons_Server(Hetcons_Server, hetcons_Server_verify_quorums
                    ,hetcons_Server_verify_proof, hetcons_Server_verify_2b
                    ,hetcons_Server_verify_2a, hetcons_Server_verify_1b
                    ,hetcons_Server_verify_1a, hetcons_Server_address_book
                    ,hetcons_Server_private_key, hetcons_Server_crypto_id
                    ,hetcons_Server_state_var) )
import Hetcons.Send ()
import Hetcons.Send_Message_IO
    ( Address_Book, default_Address_Book, send_Message_IO )
import Hetcons.Signed_Message
    ( Recursive_1b
     ,Recursive_1a
     ,Verified
     ,Recursive_2b
     ,Monad_Verify(verify)
     ,sign )
import Hetcons.Send_Message_IO ( domain_name )

import Hetcons_Consts ( sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR )
import qualified Hetcons_Observer as Observer ( process )
import qualified Hetcons_Observer_Iface as Observer
    ( ping, phase_2b )
import Hetcons_Observer_Iface ( Hetcons_Observer_Iface )
import Hetcons_Participant ( process )
import Hetcons_Participant_Iface
    ( Hetcons_Participant_Iface, ping, proposal_1a, phase_1b )
import Hetcons_Types
    ( Participant_ID(participant_ID_crypto_id, participant_ID_address)
                    ,default_Participant_ID
     ,Value(value_slot, value_value_payload)
           ,default_Value
     ,Observers(observers_observer_graph, observers_observer_quorums)
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
     ,Observer_Trust_Constraint(observer_Trust_Constraint_live
                               ,observer_Trust_Constraint_safe
                               ,observer_Trust_Constraint_observer_2
                               ,observer_Trust_Constraint_observer_1)
                               ,default_Observer_Trust_Constraint
     )

import Control.Concurrent ( forkIO, ThreadId )
import Control.Concurrent.MVar ( putMVar, takeMVar, newEmptyMVar )
import Control.Exception ( SomeException, catch )
import Crypto.Random ( getSystemDRG, DRG, withDRG )
import qualified Data.ByteString.Lazy as ByteString
    ( singleton, readFile )
import Data.ByteString.Lazy ( ByteString )
import Data.HashSet ( insert, fromList, empty )
import Data.Serialize ( Serialize )
import Test.HUnit
    ( Test(TestList, TestLabel, TestCase), assertEqual, assertBool )
import qualified Data.HashMap.Strict as HashMap ( fromList )
import Data.Text.Lazy ( pack )
import Thrift.Server ( runBasicServer )

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
  ; proof_receipt <- newEmptyMVar
  ; observer <- (basic_observer_server
                  (default_Crypto_ID {
                    crypto_ID_public_crypto_key =
                      Just (default_Public_Crypto_Key {
                              public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                  private
                  (fromIntegral new_port)
                  $ putMVar proof_receipt)
  ; (Right signed_1a) <- sample_sign $ sample_1a now [sample_id cert (fromIntegral new_port)]
  ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
  ; (Right signed_1b) <- sample_sign $ default_Phase_1b { phase_1b_proposal = signed_1a }
  ; let (Right (v1b :: (Verified Recursive_1b))) = verify signed_1b
  ; (Right signed_2b) <- sample_sign $ default_Phase_2b { phase_2b_phase_1bs = fromList [signed_1b]}
  ; let (Right (v2b :: (Verified Recursive_2b))) = verify signed_2b
  ; send_Message_IO address_book v2b
  ; assertBool "have launched an observer" True
  ; received_proof <- takeMVar proof_receipt
  ; assertEqual "incorrect observers proven" ("localhost:"++(show $ fromIntegral new_port)++",") $
      foldr (\n x -> x ++ (domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n) ++",") "" $ observers_proven received_proof
  ; return (new_port, now, address_book, cert, private)
  }

-- | Separating this out into a separate variable helps the profiler identify how much time it took.
test_4_participants =
   TestLabel "Four participant Consensus" (
     TestCase ( do
       { private <- ByteString.readFile "test/key.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; private2 <- ByteString.readFile "test/key2.pem"
       ; private3 <- ByteString.readFile "test/key3.pem"
       ; private4 <- ByteString.readFile "test/key4.pem"
       ; private6 <- ByteString.readFile "test/key6.pem"
       ; cert <- ByteString.readFile "test/cert.pem"
       ; certs' <- mapM (\i -> ByteString.readFile $ "test/cert" ++ (show i) ++ ".pem") [1..6]
       ; let (ids :: [Participant_ID]) = map (uncurry sample_id) (zip (cert:certs') [85020..85026])
       ; proof_receipt <- newEmptyMVar
       ; participant1 <- new_participant (participant_ID_crypto_id (ids!!1)) private1
       ; participant_thread1<- participant_server participant1 85021
       ; participant2 <- new_participant (participant_ID_crypto_id (ids!!2)) private2
       ; participant_thread2 <- participant_server participant2 85022
       ; participant3 <- start_State >>= (\(sv :: Participant_State_Var) -> return (participant1{hetcons_Server_state_var = sv
                                                              ,hetcons_Server_crypto_id = (participant_ID_crypto_id (ids!!3))
                                                              ,hetcons_Server_private_key = private3}))
       ; participant_thread3<- participant_server participant3 85023
       ; participant4 <- start_State >>= (\(sv :: Participant_State_Var) -> return (participant1{hetcons_Server_state_var = sv
                                                              ,hetcons_Server_crypto_id = (participant_ID_crypto_id (ids!!4))
                                                              ,hetcons_Server_private_key = private4}))
       ; participant_thread4<- participant_server participant4 85024
       ; observer_1_state <- start_State
       ; let observer1_datum = Observer {
           observer_hetcons_server = (Hetcons_Server {
                                       hetcons_Server_crypto_id = (participant_ID_crypto_id (ids!!0))
                                      ,hetcons_Server_private_key = private
                                      ,hetcons_Server_address_book = hetcons_Server_address_book participant1
                                      ,hetcons_Server_state_var = observer_1_state
                                      ,hetcons_Server_verify_1a = hetcons_Server_verify_1a participant1
                                      ,hetcons_Server_verify_1b = hetcons_Server_verify_1b participant1
                                      ,hetcons_Server_verify_2a = hetcons_Server_verify_2a participant1
                                      ,hetcons_Server_verify_2b = hetcons_Server_verify_2b participant1
                                      ,hetcons_Server_verify_proof = hetcons_Server_verify_proof participant1
                                      ,hetcons_Server_verify_quorums = hetcons_Server_verify_quorums participant1
                                      })
           ,do_on_consensus = putMVar proof_receipt}
       ; observer1 <- observer_server observer1_datum 85020
       ; observer_2_state <- start_State
       ; let observer2_datum = Observer {
           observer_hetcons_server = (Hetcons_Server {
                                       hetcons_Server_crypto_id = (participant_ID_crypto_id (ids!!6))
                                      ,hetcons_Server_private_key = private6
                                      ,hetcons_Server_address_book = hetcons_Server_address_book participant1
                                      ,hetcons_Server_state_var = observer_2_state
                                      ,hetcons_Server_verify_1a = hetcons_Server_verify_1a participant1
                                      ,hetcons_Server_verify_1b = hetcons_Server_verify_1b participant1
                                      ,hetcons_Server_verify_2a = hetcons_Server_verify_2a participant1
                                      ,hetcons_Server_verify_2b = hetcons_Server_verify_2b participant1
                                      ,hetcons_Server_verify_proof = hetcons_Server_verify_proof participant1
                                      ,hetcons_Server_verify_quorums = hetcons_Server_verify_quorums participant1
                                      })
           ,do_on_consensus = putMVar proof_receipt}
       ; observer2<- observer_server observer2_datum 85026
       ; address_book <- default_Address_Book
       ; now <- current_nanoseconds
       ; let message_1a = default_Proposal_1a {
                            proposal_1a_value = default_Value {
                                                   value_value_payload = ByteString.singleton 42
                                                  ,value_slot = 6}
                           ,proposal_1a_timestamp = now
                           ,proposal_1a_observers = Just default_Observers {
                                                           observers_observer_graph = Just $ fromList $ [
                                                                   default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3        ]
                                                                    ,observer_Trust_Constraint_live = fromList $ [ids!!1, ids!!2, ids!!3        ]}
                                                                  ,default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2,         ids!!4]
                                                                    ,observer_Trust_Constraint_live = fromList $ [ids!!1, ids!!2,         ids!!4]}
                                                                  ,default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1,         ids!!3, ids!!4]
                                                                    ,observer_Trust_Constraint_live = fromList $ [ids!!1,         ids!!3, ids!!4]}
                                                                  ,default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [        ids!!2, ids!!3, ids!!4]
                                                                    ,observer_Trust_Constraint_live = fromList $ [        ids!!2, ids!!3, ids!!4]}]}}
       ; (Right signed_1a) <- sample_sign $ message_1a
       ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book v1a)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; received_proof <- takeMVar proof_receipt
       ; assertEqual "incorrect observers proven" (fromList ["localhost:85020","localhost:85026"]) $
           foldr (\n x -> insert ((domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n)) x) empty $ observers_proven received_proof
       ; assertEqual "wrong value proven" (extract_value message_1a) (extract_value received_proof)
       ; received_proof2 <- takeMVar proof_receipt
       ; assertEqual "incorrect observers proven" (fromList ["localhost:85020","localhost:85026"]) $
           foldr (\n x -> insert ((domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n)) x) empty $ observers_proven received_proof2
       ; assertEqual "wrong value proven" (extract_value message_1a) (extract_value received_proof2)
       -- print out clock time taken with ; current_nanoseconds >>= (putStrLn . ("\n"++) . show . (\x -> (fromIntegral (x - now))/(1000000000.0)))
       -- The messages sent are:
       -- 1A:                  1 sender,  1 signer,  4 recipients, 1 instance  per signer =  4 messages
       -- 1B:                  4 senders, 4 signers, 4 recipients, 1 instance  per signer = 64 messages
       -- 2B:                  3 senders, 4 signers, 2 recipients, 2 instances per signer = 48 messages
       -- PoC featuring 3 2Bs: 2 senders, 1 signer,  2 recipients, 1 instance  per signer = 12 messages
       -- PoC featuring 4 2Bs: 2 senders, 1 signer,  2 recipients, 1 instance  per signer = 16 messages
       --                                                                           TOTAL = 144 messages
       }))

consensus_tests = TestList [

   TestLabel "One participant Consensus" (
     TestCase ( do
       { private <- ByteString.readFile "test/key.pem"
       ; cert <- ByteString.readFile "test/cert.pem"
       ; participant_thread <- (basic_participant_server
                                 (default_Crypto_ID {
                                  crypto_ID_public_crypto_key =
                                    Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                                 private
                                 85001)
       ; proof_receipt <- newEmptyMVar
       ; observer <- (basic_observer_server
                       (default_Crypto_ID {
                         crypto_ID_public_crypto_key =
                           Just (default_Public_Crypto_Key {
                                   public_Crypto_Key_public_crypto_key_x509 = Just cert})})
                       private
                       85000
                       $ putMVar proof_receipt)
       ; address_book <- default_Address_Book
       ; now <- current_nanoseconds
       ; let message_1a = default_Proposal_1a {
                            proposal_1a_value = default_Value {
                                                   value_value_payload = ByteString.singleton 42
                                                  ,value_slot = 6}
                           ,proposal_1a_timestamp = now
                           ,proposal_1a_observers = Just default_Observers {
                              observers_observer_quorums = Just $ HashMap.fromList [(sample_id cert 85000,
                                                                    fromList [fromList [sample_id cert 85001]])]}}
       ; (Right signed_1a) <- sample_sign $ message_1a
       ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book v1a)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; received_proof <- takeMVar proof_receipt
       ; assertEqual "incorrect observers proven" "localhost:85000," $
           foldr (\n x -> x ++ (domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n) ++",") "" $ observers_proven received_proof
       }))

  ,TestLabel "Three participant Consensus" (
     TestCase ( do
       { private <- ByteString.readFile "test/key.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; private2 <- ByteString.readFile "test/key2.pem"
       ; private3 <- ByteString.readFile "test/key3.pem"
       ; private6 <- ByteString.readFile "test/key6.pem"
       ; cert <- ByteString.readFile "test/cert.pem"
       ; certs' <- mapM (\i -> ByteString.readFile $ "test/cert" ++ (show i) ++ ".pem") [1..6]
       ; let (ids :: [Participant_ID]) = map (uncurry sample_id) (zip (cert:certs') [85010..85016])
       ; proof_receipt <- newEmptyMVar
       ; observer1<- (basic_observer_server
                       (participant_ID_crypto_id (ids!!0))
                       private
                       85010
                       $ putMVar proof_receipt)
       ; observer2<- (basic_observer_server
                       (participant_ID_crypto_id (ids!!6))
                       private6
                       85016
                       $ putMVar proof_receipt)
       ; participant_thread1<- (basic_participant_server
                                 (participant_ID_crypto_id (ids!!1))
                                 private1
                                 85011)
       ; participant_thread2<- (basic_participant_server
                                 (participant_ID_crypto_id (ids!!2))
                                 private2
                                 85012)
       ; participant_thread3<- (basic_participant_server
                                 (participant_ID_crypto_id (ids!!3))
                                 private3
                                 85013)
       ; address_book <- default_Address_Book
       ; now <- current_nanoseconds
       ; let message_1a = default_Proposal_1a {
                            proposal_1a_value = default_Value {
                                                   value_value_payload = ByteString.singleton 42
                                                  ,value_slot = 6}
                           ,proposal_1a_timestamp = now
                           ,proposal_1a_observers = Just default_Observers {
                                                           observers_observer_graph = Just $ fromList $ [
                                                                   default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3]
                                                                    ,observer_Trust_Constraint_live = fromList $ [ids!!1, ids!!2       ]}
                                                                  ,default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3]
                                                                    ,observer_Trust_Constraint_live = fromList $ [ids!!1,        ids!!3]}
                                                                  ,default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3]
                                                                    ,observer_Trust_Constraint_live = fromList $ [       ids!!2, ids!!3]}]}}
       ; (Right signed_1a) <- sample_sign $ message_1a
       ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book v1a)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> (assertBool ("Exception Caught: " ++ (show exception)) ((show exception) == "thread killed"))))
       ; received_proof <- takeMVar proof_receipt
       ; assertEqual "incorrect observers proven" (fromList ["localhost:85010","localhost:85016"]) $
           foldr (\n x -> insert ((domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n)) x) empty $ observers_proven received_proof
       ; assertEqual "wrong value proven" (extract_value message_1a) (extract_value received_proof)
       ; received_proof2 <- takeMVar proof_receipt
       ; assertEqual "incorrect observers proven" (fromList ["localhost:85010","localhost:85016"]) $
           foldr (\n x -> insert ((domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n)) x) empty $ observers_proven received_proof2
       ; assertEqual "wrong value proven" (extract_value message_1a) (extract_value received_proof2)
       }))

  ,test_4_participants

  ,TestLabel "Three participant Consensus with a crash failure" (
     TestCase ( do
       { private <- ByteString.readFile "test/key.pem"
       ; private1 <- ByteString.readFile "test/key1.pem"
       ; private2 <- ByteString.readFile "test/key2.pem"
       ; private3 <- ByteString.readFile "test/key3.pem"
       ; private6 <- ByteString.readFile "test/key6.pem"
       ; cert <- ByteString.readFile "test/cert.pem"
       ; certs' <- mapM (\i -> ByteString.readFile $ "test/cert" ++ (show i) ++ ".pem") [1..6]
       ; let (ids :: [Participant_ID]) = map (uncurry sample_id) (zip (cert:certs') [85030..85036])
       ; proof_receipt <- newEmptyMVar
       ; observer1<- (basic_observer_server
                       (participant_ID_crypto_id (ids!!0))
                       private
                       85030
                       $ putMVar proof_receipt)
       ; observer2<- (basic_observer_server
                       (participant_ID_crypto_id (ids!!6))
                       private6
                       85036
                       $ putMVar proof_receipt)
       ; participant_thread1<- (basic_participant_server
                                 (participant_ID_crypto_id (ids!!1))
                                 private1
                                 85031)
       ; participant_thread2<- (basic_participant_server
                                 (participant_ID_crypto_id (ids!!2))
                                 private2
                                 85032)
       -- participant 3 is dead. Specifically, it is never launched:
       -- ; participant_thread3<- (basic_participant_server
       --                           (participant_ID_crypto_id (ids!!3))
       --                           private3
       --                           85033)
       ; address_book <- default_Address_Book
       ; now <- current_nanoseconds
       ; let message_1a = default_Proposal_1a {
                            proposal_1a_value = default_Value {
                                                   value_value_payload = ByteString.singleton 42
                                                  ,value_slot = 6}
                           ,proposal_1a_timestamp = now
                           ,proposal_1a_observers = Just default_Observers {
                                                           observers_observer_graph = Just $ fromList $ [
                                                                   default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3]
                                                                    ,observer_Trust_Constraint_live = fromList $ [ids!!1, ids!!2       ]}
                                                                  ,default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3]
                                                                    ,observer_Trust_Constraint_live = fromList $ [ids!!1,        ids!!3]}
                                                                  ,default_Observer_Trust_Constraint  {
                                                                     observer_Trust_Constraint_observer_1 = ids!!0
                                                                    ,observer_Trust_Constraint_observer_2 = ids!!6
                                                                    ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3]
                                                                    ,observer_Trust_Constraint_live = fromList $ [       ids!!2, ids!!3]}]}}
       ; (Right signed_1a) <- sample_sign $ message_1a
       ; let (Right (v1a :: (Verified Recursive_1a))) = verify signed_1a
        -- There is a crash failure, so the send thread will actually get an Exception from Thrift.
        -- However, this should not interfere with Consensus beign reached.
       ; send_thread <- forkIO (catch (catch (send_Message_IO address_book v1a)
                                      (\(exception :: Hetcons_Exception) -> assertBool ("Hetcons Exception Caught: " ++ (show exception)) False))
                               (\(exception :: SomeException) -> return ()))
       ; received_proof <- takeMVar proof_receipt
       ; assertEqual "incorrect observers proven" (fromList ["localhost:85030","localhost:85036"]) $
           foldr (\n x -> insert ((domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n)) x) empty $ observers_proven received_proof
       ; assertEqual "wrong value proven" (extract_value message_1a) (extract_value received_proof)
       ; received_proof2 <- takeMVar proof_receipt
       ; assertEqual "incorrect observers proven" (fromList ["localhost:85030","localhost:85036"]) $
           foldr (\n x -> insert ((domain_name n) ++ ":"++ (show $ address_port_number $ participant_ID_address n)) x) empty $ observers_proven received_proof2
       ; assertEqual "wrong value proven" (extract_value message_1a) (extract_value received_proof2)
       }))

  ]

