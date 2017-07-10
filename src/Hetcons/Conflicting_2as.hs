
module Hetcons.Conflicting_2as (conflicting_2as) where

import Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
    , Contains_1a
        ,extract_1a
        ,extract_observer_quorums
    , Ballot
        ,extract_ballot
    ,Contains_1bs
        ,extract_1bs
    )
import Hetcons.Instances_1b_2a (well_formed_2a)
import Hetcons.Signed_Message (Verified, Recursive_1b, Recursive_2a(Recursive_2a), non_recursive)
import Hetcons.Value (conflicts)

import Hetcons_Types (Phase_2a)

import Data.Foldable (toList, length)
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashSet (HashSet, empty, fromList, singleton, insert)



-- | returns Phase_2as with identical quorums to the message, but conflicting values
conflicting_2as :: (Contains_1a a, Contains_Value a, Foldable f) => (f (Verified Recursive_1b)) -> a -> (HashSet Phase_2a)
conflicting_2as old_1bs new_message =
  let quorums_1bs = fromList $ filter (conflicts . (\x -> insert x $ singleton $ extract_value new_message) . extract_value) $ -- TODO: non-pairwise conflicts
                               filter (((extract_observer_quorums new_message) ==) . extract_observer_quorums) $ toList old_1bs -- same quorums
      with_both = HashSet.map (\x -> HashSet.filter (\y -> (((extract_value x) == (extract_value y)) && (((extract_1a x) == (extract_1a y))))) quorums_1bs) quorums_1bs
      potential_2as = HashSet.map Recursive_2a with_both
   in HashSet.map non_recursive $ HashSet.filter (\x -> case well_formed_2a x of
                                                          Left _ -> False
                                                          Right _ -> True) potential_2as
