module Hetcons.Phase_2a
    ( get_2as
    ) where

import Hetcons.Contains_Value (Contains_Value
                                 ,extract_value)
import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Signed_Message (Recursive_1b
                                 ,recursive_1b_proposal
                              ,Recursive_1a(Recursive_1a)
                                 ,recursive_1a_filled_in
                              ,Recursive_2a (Recursive_2a )
                              ,Parsable
                              ,Verified
                                 ,original
                                 ,signed
                              )

import Hetcons_Consts ()
import Hetcons_Types  (Value
                         ,value_slot
                      ,Proposal_1a (Proposal_1a)
                         ,proposal_1a_value
                         ,proposal_1a_observers
                      ,Signed_Message
                         ,signed_Message_signature
                      ,Signed_Hash
                         ,signed_Hash_crypto_id
                      ,Crypto_ID
                      ,Participant_ID (Participant_ID)
                         ,participant_ID_crypto_id
                      ,Observers (Observers)
                         ,observers_observer_graph
                         ,observers_observer_quorums
                      )

import qualified Data.Foldable as Foldable (any)
import Data.Foldable (toList, length)
import qualified Data.HashMap.Lazy as HashMap (filter)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap, mapWithKey, filterWithKey, elems)
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashSet (HashSet
                       ,member
                       ,intersection
                       ,fromList
                       ,toMap
                       ,unions
                       )
import Data.List (head)
import Data.Maybe (catMaybes)

quorums :: Recursive_1a -> (HashSet (HashSet Participant_ID))
quorums (Recursive_1a { recursive_1a_filled_in = Proposal_1a {
         proposal_1a_observers = Just
           Observers {
           observers_observer_quorums = Just x}}})
  = unions $ elems x

subset :: (Eq a, Hashable a) =>  HashSet a -> HashSet a -> Bool
subset x y = (intersection x y) == x

get_2as :: (HashSet (Verified Recursive_1b)) -> (HashSet Recursive_2a)
get_2as s = let proposals = HashSet.map (original . recursive_1b_proposal . original) s
                phase_1bs_by_proposal = mapWithKey (\v _ -> HashSet.filter ((v ==) . original . recursive_1b_proposal . original) s)
                                                   $ toMap proposals
                quorums_by_proposal = filterWithKey (\v s -> Foldable.any (\q -> subset (HashSet.map participant_ID_crypto_id q)
                  ((fromList.catMaybes.toList) (HashSet.map (signed_Hash_crypto_id.signed_Message_signature.signed) s)))
                  $ quorums v)
                  phase_1bs_by_proposal
             in fromList $ map Recursive_2a $ elems quorums_by_proposal
