
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
import Hetcons.Signed_Message (Verified, Recursive_1b)

import Hetcons_Types (Phase_2a)

import Data.HashSet (HashSet, empty)



-- | TODO: implement this for real
conflicting_2as :: (Contains_1a a, Contains_Value a, Foldable f) => (f (Verified Recursive_1b)) -> a -> (HashSet Phase_2a)
conflicting_2as _ _ = empty