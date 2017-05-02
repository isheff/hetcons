module Hetcons.Value
    ( conflicts
    , garbage_collect
    ) where

import Hetcons.Contains_Value (Contains_Value
                                 ,extract_value)
import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Signed_Message (Recursive_1b, Verified)

import Hetcons_Types  (Value
                         ,value_slot)

import Data.Foldable (length)
import qualified Data.HashSet as HashSet (map)
import Data.HashSet (HashSet)




-- | Does this set of entities contain conflicitng values?
-- | In this case, do any two of them have the same value_slot?
conflicts :: (Contains_Value a) => (HashSet a) -> Bool
conflicts s = (length s) /= (length (HashSet.map (value_slot . extract_value) s))

-- | TODO: figure out what should even be done here
garbage_collect :: (HashSet (Verified Recursive_1b)) -> (HashSet (Verified Recursive_1b))
garbage_collect = id
