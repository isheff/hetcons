{-# LANGUAGE OverloadedStrings #-}
module Test.Quorums (quorums_tests) where

import Hetcons.Contains_Value (extract_1a)
import Hetcons.Hetcons_Exception (Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                                                   ,Hetcons_Exception_Unparsable_Hashable_Message
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                                                   ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Invalid_Signed_Hash))

import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Signed_Message (Verified
                                 ,signed
                                 ,original
                              ,sign
                              ,verify
                              ,Recursive_1a
                              ,recursive_1a_filled_in
                              ,Recursive_2a (Recursive_2a)
                              ,Recursive_2b (Recursive_2b)
                              ,Recursive_Proof_of_Consensus
                              ,recursive_1b_proposal
                              ,recursive_1b_conflicting_phase2as
                              ,Parsable
                              ,non_recursive)


import Hetcons_Consts(sUPPORTED_HASH_SHA2_DESCRIPTOR
                     ,sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR)

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
                     ,Observers(Observers)
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
                     )

import           Control.Monad (join)
import           Control.Monad.Except   (runExceptT)
import Control.Monad.Trans.Except (except)
import Crypto.Random (getSystemDRG, DRG, withDRG)
import qualified Data.ByteString.Lazy as ByteString (readFile, concat, take, drop, singleton, index)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Combinators (isLeft, isRight)
import Data.Either.Combinators (mapRight)
import qualified Data.HashMap.Lazy  as HashMap (fromList, toList)
import           Data.HashMap.Lazy           (empty)
import           Data.HashSet           (fromList,toList)
import Data.List (head, elemIndex, sort)
import           Data.Serialize         (Serialize
                                           ,decodeLazy)
import Test.HUnit (Test(TestList,
                        TestLabel,
                        TestCase)
                  ,assertEqual
                  ,assertBool)
import Data.HashMap.Strict (singleton)
import           Data.Text.Lazy         (pack)




fill_in_observers :: Observers -> IO Observers
fill_in_observers observers =
  do { cert <- ByteString.readFile "test/cert.pem"
     ; let sample = ((sample_1a cert) {proposal_1a_observers = Just observers})
     ; signed <- sample_sign $ sample
     ; let verified = mapRight ((mapRight ((non_recursive :: Recursive_1a -> Proposal_1a).original)).verify) signed
     ; assertEqual "failed to verify a signed proposal_1a" (Right $ Right sample) verified
     ; let answer = do { s <- signed
                       ; v_r1a <- verify s
                       ; return $ proposal_1a_observers $ recursive_1a_filled_in $ original v_r1a}
     ; assertBool "Exception while parsing signed Proposal_1a" $ isRight answer
     ; return ((\(Right (Just x)) -> x) answer)}


doubleGen :: (DRG g) => g -> (g,g)
doubleGen g = withDRG g (return g)

listGen :: (DRG g) => g -> [g]
listGen g = g:(listGen (snd (withDRG g (return ()))))





sample_sign :: (Serialize a) => a -> IO (Either Hetcons_Exception Signed_Message)
sample_sign payload =
  do { gen <- getSystemDRG
     ; cert <- ByteString.readFile "test/cert.pem"
     ; private <- ByteString.readFile "test/key.pem"
     ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert})}
     ; return $ sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen payload}

sample_id :: ByteString -> Participant_ID
sample_id cert =
  default_Participant_ID  {
    participant_ID_address =
      default_Address  {
        address_host_address =
          default_Host_Address  {
            host_Address_dns_name = Just $ pack "localhost"}
       ,address_port_number = 8976}
   ,participant_ID_crypto_id =
      default_Crypto_ID {
        crypto_ID_public_crypto_key =
          Just (default_Public_Crypto_Key {
                  public_Crypto_Key_public_crypto_key_x509 = Just cert})}}

-- sample_1a :: Proposal_1a
sample_1a cert = default_Proposal_1a {
   proposal_1a_value = default_Value {
                          value_value_payload = ByteString.singleton 42
                         ,value_slot = 6}
  ,proposal_1a_timestamp = 1111111
  ,proposal_1a_observers = Just default_Observers {
     observers_observer_quorums = Just $ singleton (sample_id cert) (fromList [ fromList [sample_id cert]])}}

-- | Try the quorum creation algorithms by inputing a graph
-- | for ease of use, we have our own little language here for observer graphs
-- | using Ints 0..6 to stand for ids, input constraints of the form (observer, observer, [safe], [live])
-- | and a correct graph of the form [(observer [quorum of participants :: [id]])]
-- | and this will run an end-to-end test of quorum creation, and see if it comes out correctly
test_quorum_creation :: [(Int, Int, [Int], [Int])] -> [(Int, [[Int]])] -> IO ()
test_quorum_creation constraints correct_quorums =
  do { cert <- ByteString.readFile "test/cert.pem"
     ; certs' <- mapM (\i -> ByteString.readFile $ "test/cert" ++ (show i) ++ ".pem") [1..9]
     ; let certs = cert:certs'
     ; let ids = map sample_id certs
     ; let observers = default_Observers {observers_observer_graph = Just $ fromList $ map
           (\(id1, id2, safe, live) ->
               default_Observer_Trust_Constraint  {
                 observer_Trust_Constraint_observer_1 = ids!!id1
                ,observer_Trust_Constraint_observer_2 = ids!!id2
                ,observer_Trust_Constraint_safe = fromList $ map (ids!!) safe
                ,observer_Trust_Constraint_live = fromList $ map (ids!!) live})
           constraints}
     ; observers' <- fill_in_observers observers
           -- prettier when printed:
     ; let observers_list = sort $ map (\(oid, qs) -> ((\(Just x) -> x) $
             elemIndex oid ids, sort $ map (\q -> sort $ map (\x -> (\(Just y) -> y) $ elemIndex x ids) $
             toList q) $ toList qs)) $ HashMap.toList $ (\(Observers {observers_observer_quorums = Just x}) -> x) observers'
     ; assertEqual "incorrectly filled in quorums"
                   (sort $ map (\(x,y) -> (x, sort $ map (sort . (map fromIntegral)) y)) correct_quorums)
                   observers_list
    ; return ()}


quorums_tests = TestList [

   TestLabel "single observer, single participant" (
     TestCase ( test_quorum_creation [(1,1,[1],[1])] [(1,[[1]])] ))

  ,TestLabel "two observer, four participant" (
     TestCase ( test_quorum_creation [  (1,2,[1,2,3  ],[1,2,3  ])
                                       ,(1,2,[1,2,  4],[1,2,  4])
                                       ,(1,2,[1,  3,4],[1,  3,4])
                                       ,(1,2,[  2,3,4],[  2,3,4])
                                     ]
                                     [  (1,[[1,2,3,4]
                                           ,[1,2,3  ]
                                           ,[1,2  ,4]
                                           ,[1  ,3,4]
                                           ,[  2,3,4]
                                           ])
                                       ,(2,[[1,2,3,4]
                                           ,[1,2,3  ]
                                           ,[1,2  ,4]
                                           ,[1  ,3,4]
                                           ,[  2,3,4]
                                           ])
                                     ] ))

  ,TestLabel "two observer, three participant" (
     TestCase ( test_quorum_creation [  (1,2,[1,2,3],[1,2  ])
                                       ,(1,2,[1,2,3],[1,  3])
                                       ,(1,2,[1,2,3],[  2,3])
                                     ]
                                     [  (1,[[1,2,3]
                                           ,[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(2,[[1,2,3]
                                           ,[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                     ] ))

   -- This passes, but is crazy slow
  ,TestLabel "two observer, nine participant (3 groups)" (
     TestCase ( test_quorum_creation [  (1,2,[1,2,3,4,5,6,7    ],[1,2,3,4,5,6      ])
                                       ,(1,2,[1,2,3,4,5,6,  8  ],[1,2,3,4,5,6      ])
                                       ,(1,2,[1,2,3,4,5,6,    9],[1,2,3,4,5,6      ])
                                       ,(1,2,[1,2,3,4,    7,8,9],[1,2,3,      7,8,9])
                                       ,(1,2,[1,2,3,  5,  7,8,9],[1,2,3,      7,8,9])
                                       ,(1,2,[1,2,3,    6,7,8,9],[1,2,3,      7,8,9])
                                       ,(1,2,[1,    4,5,6,7,8,9],[      4,5,6,7,8,9])
                                       ,(1,2,[  2,  4,5,6,7,8,9],[      4,5,6,7,8,9])
                                       ,(1,2,[    3,4,5,6,7,8,9],[      4,5,6,7,8,9])
                                     ]
                                     [  (1,[[1,2,3,4,5,6,7,8,9]
                                           ,[1,2,3,4,5,6      ]
                                           ,[1,2,3,4,5,6,  8,9]
                                           ,[1,2,3,4,5,6,7,  9]
                                           ,[1,2,3,4,5,6,7,8  ]
                                           ,[1,2,3,4,5,6,7    ]
                                           ,[1,2,3,4,5,6,  8  ]
                                           ,[1,2,3,4,5,6,    9]
                                           ,[1,2,3,      7,8,9]
                                           ,[1,2,3,  5,6,7,8,9]
                                           ,[1,2,3,4,  6,7,8,9]
                                           ,[1,2,3,4,5,  7,8,9]
                                           ,[1,2,3,4,    7,8,9]
                                           ,[1,2,3,  5,  7,8,9]
                                           ,[1,2,3,    6,7,8,9]
                                           ,[      4,5,6,7,8,9]
                                           ,[  2,3,4,5,6,7,8,9]
                                           ,[1,  3,4,5,6,7,8,9]
                                           ,[1,2,  4,5,6,7,8,9]
                                           ,[1,    4,5,6,7,8,9]
                                           ,[  2,  4,5,6,7,8,9]
                                           ,[    3,4,5,6,7,8,9]
                                           ])
                                       ,(2,[[1,2,3,4,5,6,7,8,9]
                                           ,[1,2,3,4,5,6      ]
                                           ,[1,2,3,4,5,6,  8,9]
                                           ,[1,2,3,4,5,6,7,  9]
                                           ,[1,2,3,4,5,6,7,8  ]
                                           ,[1,2,3,4,5,6,7    ]
                                           ,[1,2,3,4,5,6,  8  ]
                                           ,[1,2,3,4,5,6,    9]
                                           ,[1,2,3,      7,8,9]
                                           ,[1,2,3,  5,6,7,8,9]
                                           ,[1,2,3,4,  6,7,8,9]
                                           ,[1,2,3,4,5,  7,8,9]
                                           ,[1,2,3,4,    7,8,9]
                                           ,[1,2,3,  5,  7,8,9]
                                           ,[1,2,3,    6,7,8,9]
                                           ,[      4,5,6,7,8,9]
                                           ,[  2,3,4,5,6,7,8,9]
                                           ,[1,  3,4,5,6,7,8,9]
                                           ,[1,2,  4,5,6,7,8,9]
                                           ,[1,    4,5,6,7,8,9]
                                           ,[  2,  4,5,6,7,8,9]
                                           ,[    3,4,5,6,7,8,9]
                                           ])
                                     ] ))

  ,TestLabel "three observer, three participant" (
     TestCase ( test_quorum_creation [  (1,2,[1,2,3],[1,2  ])
                                       ,(1,2,[1,2,3],[1,  3])
                                       ,(1,2,[1,2,3],[  2,3])
                                       ,(1,3,[1,2,3],[1,2  ])
                                       ,(1,3,[1,2,3],[1,  3])
                                       ,(1,3,[1,2,3],[  2,3])
                                     ]
                                     [  (1,[[1,2,3]
                                           ,[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(2,[[1,2,3]
                                           ,[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(3,[[1,2,3]
                                           ,[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                     ] ))

  ,TestLabel "three observer, three participant asymmetric" (
     TestCase ( test_quorum_creation [  (1,2,[1,2,3],[1,2  ])
                                       ,(1,2,[1,2,3],[  2,3])
                                       ,(1,3,[1,2,3],[1,  3])
                                       ,(1,3,[1,2,3],[  2,3])
                                     ]
                                     [  (1,[[1,2,3]
                                           ,[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(2,[[1,2,3]
                                           ,[1,2  ]
                                           ,[  2,3]
                                           ])
                                       ,(3,[[1,2,3]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                     ] ))

  ]

