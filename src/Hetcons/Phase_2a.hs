module Hetcons.Phase_2a
    ( conflicting_2as
    , potential_2as
    ) where

import Hetcons.Contains_Value (Contains_Value
                                 ,extract_value
                              ,Contains_1a
                                 ,extract_1a
                                 ,extract_observer_quorums
                              ,Ballot
                                 ,extract_ballot)
import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Hetcons_State (state_by_observers)
import Hetcons.Signed_Message (Recursive_1b
                                 ,recursive_1b_proposal
                                 ,recursive_1b_conflicting_phase2as
                              ,Recursive_1a(Recursive_1a)
                                 ,recursive_1a_filled_in
                              ,Recursive_2a (Recursive_2a )
                              ,Parsable
                              ,Verified
                                 ,original
                                 ,signed
                              )
import Hetcons.Value (conflicts)

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

import qualified Data.Foldable as Foldable (any,all,foldr)
import Data.Foldable (toList, length, maximumBy)
import qualified Data.HashMap.Lazy as HashMap (filter)
import Data.Hashable (Hashable)
import Data.HashMap (filterWithKey, (!))
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashSet (HashSet
                       ,member
                       ,intersection
                       ,fromList
                       ,unions
                       ,singleton
                       ,empty
                       ,filter
                       ,null
                       ,toMap
                       )
import Data.List (head)
import Data.Maybe (catMaybes)
import Data.Ord (compare)

-- | Assembles 2as from the given 1bs that share the same BALLOT and COG (nothing to do with quorums)
potential_2as  ::  (HashSet (Verified Recursive_1b)) -> (HashSet (Recursive_2a))
potential_2as received_1bs =
  (HashSet.map (\phase_1b -> (Recursive_2a -- 2as assembled from all existing 1bs that share the same BALLOT and COG
    (HashSet.filter
      (\x -> (((extract_ballot phase_1b) == (extract_ballot x)) &&
              (((proposal_1a_observers . extract_1a) phase_1b) == ((proposal_1a_observers . extract_1a) x))))
      received_1bs)))
    received_1bs)


-- TODO: I don't think this is correct.
value_2a :: (Contains_1a a) => a -> Crypto_ID -> Recursive_2a -> Maybe Value
value_2a contains_quorums observer (Recursive_2a set_of_1bs) =
  let quorums = HashSet.map (HashSet.map participant_ID_crypto_id) $ head $ elems $
        filterWithKey ((observer ==) . participant_ID_crypto_id . original) $ extract_observer_quorums contains_quorums
        nonrecursive_value = extract_value $ extract_1a $ head $ toList set_of_1bs
      genuinely_conflicts x = (case value_2a contains_quorums observer (original x) of
                                    Nothing -> False
                                    Just v -> ((v /= nonrecursive_value) && (conflicts $ fromList [v, nonrecursive_value])))
      no_2a_1bs = (HashSet.filter
        (\this_1b -> (null $ recursive_1b_conflicting_phase2as $ original this_1b) || -- either it has no 2as in it
          (all (not . genuinely_conflicts) $ recursive_1b_conflicting_phase2as $ original this_1b)) -- or none of them conflict with the 1bs
        set_of_1bs)
      no_2a_1bs_crypto_ids = (HashSet.map ((fromList.catMaybes.toList).(HashSet.map (signed_Hash_crypto_id.signed_Message_signature.signed)))
                                          no_2a_1bs)
      greatest_ballot_2a = maximumBy (\x y -> compare (extract_ballot x) (extract_ballot y))
      recurse_2as = unions $ toList $ HashSet.map (recursive_1b_conflicting_phase2as . original) set_of_1bs
      genuine_conflicts = HashSet.filter genuinely_conflicts recurse_2as
   in if any (\quorum -> (intersection quorum no_2a_1bs_crypto_ids) == quorum) quorums -- if there's a quorum of 1bs without 2as in them
         then Just nonrecursive_value
         else if null genuine_conflicts -- no quorum of 1bs without 2as, no other 1bs, so there's just no quorum
                 then Nothing
                 else value_2a contains_quorums observer $ greatest_ballot_2a genuine_conflicts -- the "most recent" conflict


{--
value_2a  :: (Contains_1a a) => a -> Crypto_ID -> Recursive_2a -> Maybe Value
value_2a contains_quorums observer (Recursive_2a set_of_1bs) =
  let collect_1bs x = unions (x:(toList (HashSet.map (collect_1bs . (\Recursive_2a y -> y) . original)
                                                     $ unions $ toList $ HashSet.map (recursive_1b_conflicting_phase2as . original) x)))
      all_1bs = collect_1bs set_of_1bs
      nonrecursive_value = extract_value $ extract_1a $ head $ toList set_of_1bs
      genuinely_conflicts x = (case value_2a contains_quorums observer (original x) of
                                    Nothing -> False
                                    Just v -> ((v /= nonrecursive_value) && (conflicts $ fromList [v, nonrecursive_value])))
      non_recursive_1bs = (HashSet.filter
        (\this_1b -> (null $ recursive_1b_conflicting_phase2as $ original this_1b) || -- either it has no 2as in it
          (all (not . genuinely_conflicts) $ recursive_1b_conflicting_phase2as $ original this_1b))
        all_1bs)
      quorums = HashSet.map (HashSet.map participant_ID_crypto_id) $ head $ elems $
        filterWithKey (\k _ -> ((observer ==) . participant_ID_crypto_id . original) k) $ extract_observer_quorums contains_quorums
      non_recursive_1bs_by_ballot =(HashSet.map (\k -> HashSet.filter (((extract_ballot k) ==) . extract_ballot)  non_recursive_1bs)
                                                non_recursive_1bs)
      quorum_1bs_by_ballot =(
        HashSet.filter
          (\q -> any (\quorum -> (intersection quorum
            (((fromList.catMaybes.toList).(HashSet.map (signed_Hash_crypto_id.signed_Message_signature.signed))) q)) == quorum)
            quorums)
          non_recursive_1bs_by_ballot)
   in if null quorum_1bs_by_ballot
         then Nothing
         else Just $ extract_value $ extract_1a $ maximumBy (\x y -> compare (extract_ballot x) (extract_ballot y))
                                                            $ HashSet.map (head . toList) quorum_1bs_by_ballot
--}





-- | Which subsets of the 1bs we've received so far constitute 2as that have values which conflict with
-- |  (not necessarily with higher ballot numbers) the given message (1a)?
-- | This would include a 2a for the message given, assuming the value "conflicts with" itself.
-- | At present, this only does pairwise conflict detection, because I'm not sure how to be more complex.
--     TODO: non-pairwise conflict detection
conflicting_2as :: Recursive_1a -> (HashSet (Verified Recursive_1b)) -> (HashSet (Recursive_2a))
conflicting_2as message received_1bs =
  let conflictors = (HashSet.filter -- 2as which have values conflicting with the given one, but may not satisfy quorums
                      (\phase_2a -> (conflicts $ fromList [extract_value message, extract_value phase_2a])) -- NOTE: PAIRWISE
                      $ potential_2as received_1bs)
      Proposal_1a {proposal_1a_observers = Just Observers {observers_observer_quorums = Just quorums}} = recursive_1a_filled_in message
      crypto_id_quorums = HashSet.map (HashSet.map participant_ID_crypto_id) $ Foldable.foldr intersection empty quorums  -- any quorum of any observer
   in (HashSet.filter
        (\(Recursive_2a x) ->
          let quorum_from_2a = ((fromList.catMaybes.toList).(HashSet.map (signed_Hash_crypto_id.signed_Message_signature.signed))) x
           in any (\quorum -> intersection quorum quorum_from_2a == quorum) crypto_id_quorums) -- NOTE: WE USE == ON CRYPTO_IDs
        conflictors)


