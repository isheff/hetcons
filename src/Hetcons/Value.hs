-- | Defines the two properties of a Value, a datatype defined in Thrift.
module Hetcons.Value
    ( conflicts
    , garbage_collect
    ) where

import Hetcons.Contains_Value (Contains_Value
                                 ,extract_value)
import Hetcons.Signed_Message (Recursive_1b, Verified)

import Hetcons_Types  (Value
                         ,value_slot)

import Data.Foldable (length)
import qualified Data.HashSet as HashSet (map)
import Data.HashSet (HashSet)




-- | Does this set of entities contain conflicitng values?
-- | In this case, do any two of them have the same value_slot?
conflicts :: (Contains_Value a) => (HashSet a) -> Bool
-- Are there the same number of values in this set as there are distinct slot numbers?
conflicts s = (length s) /= (length (HashSet.map (value_slot . extract_value) s))

-- | The state of a Participant maintains a set of known received Recursive_1bs.
-- | Each time it is saved, this is run.
-- | If there is ever a message we can just forget about, we should do that here.
-- | TODO: figure out what should even be done here
garbage_collect :: (HashSet (Verified Recursive_1b)) -> (HashSet (Verified Recursive_1b))
garbage_collect = id
