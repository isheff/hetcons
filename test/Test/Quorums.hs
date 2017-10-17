{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Quorums (quorums_tests) where

import Hetcons.Hetcons_Exception ( Hetcons_Exception )
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,Recursive_1a(recursive_1a_filled_in)
     ,Recursive(non_recursive)
     ,Monad_Verify(verify)
     ,Verified
     ,sign
     ,original )
import Test.Util ()

import Charlotte_Consts ( sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR )
import Charlotte_Types
    ( Participant_ID(participant_ID_crypto_id, participant_ID_address)
                    ,default_Participant_ID
     ,Slot_Value(slot_Value_slot, slot_Value_value_payload)
           ,default_Slot_Value
     ,Observers(Observers
               ,observers_observer_graph
               ,observers_observer_quorums)
               ,default_Observers
     ,Proposal_1a(proposal_1a_timestamp, proposal_1a_value
                 ,proposal_1a_observers)
                 ,default_Proposal_1a
     ,Public_Crypto_Key(public_Crypto_Key_public_crypto_key_x509)
                       ,default_Public_Crypto_Key
     ,Crypto_ID(crypto_ID_public_crypto_key)
               ,default_Crypto_ID
     ,Signed_Message
     ,Host_Address(host_Address_dns_name)
                  ,default_Host_Address
     ,Observer_Trust_Constraint(observer_Trust_Constraint_live
                               ,observer_Trust_Constraint_safe
                               ,observer_Trust_Constraint_observer_2
                               ,observer_Trust_Constraint_observer_1)
                               ,default_Observer_Trust_Constraint
     ,Address(address_port_number, address_host_address)
             ,default_Address )

import Crypto.Random ( getSystemDRG, DRG, withDRG )
import qualified Data.ByteString.Lazy as ByteString
    ( singleton, readFile )
import Data.ByteString.Lazy ( ByteString )
import Data.Either.Combinators ( isRight )
import Data.Either.Combinators ( mapRight )
import qualified Data.HashMap.Lazy as HashMap ( toList )
import Data.HashMap.Lazy ()
import Data.HashSet ( fromList, toList )
import Data.List ( sort, elemIndex )
import Test.HUnit
    ( Test(TestList, TestLabel, TestCase), assertEqual, assertBool )
import Data.HashMap.Strict ( singleton )
import Data.Text.Lazy ( pack )


fill_in_observers :: Observers -> IO Observers
fill_in_observers observers =
  do { cert <- ByteString.readFile "test/cert.pem"
     ; let sample = ((sample_1a cert) {proposal_1a_observers = Just observers})
     ; signed <- sample_sign $ sample
     ; let verified = mapRight ((mapRight ((non_recursive :: (Recursive_1a Slot_Value) -> Proposal_1a).original)).verify) signed
     ; assertEqual "failed to verify a signed proposal_1a" (Right $ Right sample) verified
     ; let answer = do { s <- signed
                       ; (v_r1a :: Verified (Recursive_1a Slot_Value)) <- verify s
                       ; return $ proposal_1a_observers $ recursive_1a_filled_in $ original v_r1a}
     ; assertBool "Exception while parsing signed Proposal_1a" $ isRight answer
     ; return ((\(Right (Just x)) -> x) answer)}


doubleGen :: (DRG g) => g -> (g,g)
doubleGen g = withDRG g (return g)

listGen :: (DRG g) => g -> [g]
listGen g = g:(listGen (snd (withDRG g (return ()))))





sample_sign :: (Encodable a) => a -> IO (Either Hetcons_Exception Signed_Message)
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
   proposal_1a_value = encode default_Slot_Value {
                          slot_Value_value_payload = ByteString.singleton 42
                         ,slot_Value_slot = 6}
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
                                     [  (1,[[1,2,3  ]
                                           ,[1,2  ,4]
                                           ,[1  ,3,4]
                                           ,[  2,3,4]
                                           ])
                                       ,(2,[[1,2,3  ]
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
                                     [  (1,[[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(2,[[1,2  ]
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
                                     [  (1,[[1,2,3,4,5,6      ]
                                           ,[1,2,3,      7,8,9]
                                           ,[      4,5,6,7,8,9]
                                           ])
                                       ,(2,[[1,2,3,4,5,6      ]
                                           ,[1,2,3,      7,8,9]
                                           ,[      4,5,6,7,8,9]
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
                                     [  (1,[[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(2,[[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(3,[[1,2  ]
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
                                     [  (1,[[1,2  ]
                                           ,[1  ,3]
                                           ,[  2,3]
                                           ])
                                       ,(2,[[1,2  ]
                                           ,[  2,3]
                                           ])
                                       ,(3,[[1  ,3]
                                           ,[  2,3]
                                           ])
                                     ] ))

  ]

