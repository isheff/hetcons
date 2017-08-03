
-- | A module for any and all functions pertaining to creating / discovering 2A messages from the set of past received 1Bs, that conflict with something.
module Hetcons.Conflicting_2as (conflicting_2as) where

import Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
    , Contains_1a
        ,extract_1a
        ,extract_observer_quorums
    )
import Hetcons.Instances_1b_2a ( well_formed_2a )
import Hetcons.Signed_Message
    ( Verified
     ,Recursive_1b
     ,Recursive_2a(Recursive_2a)
     ,non_recursive )
import Hetcons.Value ( conflicts )

import Hetcons_Types ( Phase_2a )

import Data.Foldable ( Foldable(toList) )
import qualified Data.HashSet as HashSet ( map, filter )
import Data.HashSet ( HashSet, singleton, insert, fromList )





-- | Phase_2as with identical quorums to the message, but conflicting values
-- | takes in old, known 1bs, and a new message, and filters the old 1Bs by quorums, and then by conflicts, and tests to see if they are a valid 2A
-- | NOTE: only pariwise conflicts are handled.
conflicting_2as :: (Contains_1a a, Contains_Value a, Foldable f) => (f (Verified Recursive_1b)) -> a -> (HashSet Phase_2a)
conflicting_2as old_1bs new_message =
  let quorums_1bs = fromList $ filter (conflicts . (\x -> insert x $ singleton $ extract_value new_message) . extract_value) $ -- TODO: non-pairwise conflicts
                               filter (((extract_observer_quorums new_message) ==) . extract_observer_quorums) $ toList old_1bs -- same quorums
      with_both = HashSet.map (\x -> HashSet.filter (\y -> (((extract_value x) == (extract_value y)) && (((extract_1a x) == (extract_1a y))))) quorums_1bs) quorums_1bs
      potential_2as = HashSet.map Recursive_2a with_both
   in HashSet.map non_recursive $ HashSet.filter (\x -> case well_formed_2a x of
                                                          Left _ -> False
                                                          Right _ -> True) potential_2as
