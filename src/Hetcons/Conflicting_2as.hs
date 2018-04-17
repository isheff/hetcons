{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for any and all functions pertaining to creating / discovering 2A messages from the set of past received 1Bs, that conflict with something.
module Hetcons.Conflicting_2as (conflicting_2as) where

import Hetcons.Instances_1b_2a ( well_formed_2a )
import Hetcons.Signed_Message
    ( Verified
     ,Recursive_1a
     ,Recursive_1b(Recursive_1b)
       ,recursive_1b_proposal
       ,recursive_1b_conflicting_phase2as
     ,Recursive_2a(Recursive_2a)
     ,signed
     ,original)
import Hetcons.Value
    ( Contains_Value
        ,extract_value
    , Contains_1a
        ,extract_1a
        ,extract_observer_quorums
    , Value
        ,conflicts
    )

import Charlotte_Types ( Phase_2a
                      ,default_Phase_1b
                      ,phase_1b_proposal
                      ,phase_1b_conflicting_phase2as
                      ,default_Hetcons_Message)

import Data.Foldable ( Foldable(toList) )
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet ( map, filter )
import Data.HashSet ( HashSet, singleton, insert, fromList, empty )





-- | Phase_2as with identical quorums to the message, but conflicting values
--   takes in old, known 1bs, and a new message, and filters the old 1Bs by quorums, and then by conflicts, and tests to see if they are a valid 2A
--   NOTE: only pariwise conflicts are handled.
conflicting_2as :: forall v f . (Value v, Eq v, Hashable v, Foldable f) => (f (Verified (Recursive_1b v))) -> (Verified (Recursive_1a v)) -> (HashSet (Recursive_2a v))
conflicting_2as old_1bs new_message =
  let naive_1b = Recursive_1b {
                   recursive_1b_proposal = new_message
                  ,recursive_1b_conflicting_phase2as = empty}
      quorums_1bs = filter (conflicts . (\y -> fromList [naive_1b, original y])) $ toList old_1bs -- TODO: non-pairwise conflicts
      with_both = map (\x -> filter (\y -> ((((extract_value x) :: v) == ((extract_value y) :: v)) &&
                                           ((((extract_1a x) :: Verified (Recursive_1a v)) == ((extract_1a y) :: Verified (Recursive_1a v)))))) quorums_1bs) quorums_1bs
      potential_2as = map (Recursive_2a . fromList) with_both
   in fromList $ filter (\x -> case well_formed_2a default_Hetcons_Message x of
                                    Left _ -> False
                                    Right _ -> True) potential_2as
