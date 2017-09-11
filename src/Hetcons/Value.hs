{-# LANGUAGE FlexibleContexts #-}
-- | Defines the three properties of any Value type we can Consent on, including instantiations fro Slot_Value, from Thrift.
module Hetcons.Value
    ( Value
    , valid
    , conflicts
    , garbage_collect
    ) where

import Hetcons.Contains_Value (Contains_Value
                                ,extract_value
                              ,Contains_1a
                                ,extract_observer_quorums)
import Hetcons.Signed_Message (Encodable, Parsable, Recursive_1a, Recursive_1b, Verified)

import Hetcons_Types  (Slot_Value
                         ,value_slot
                         ,encode_Slot_Value)

import Data.Foldable (length)
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashSet (HashSet, fromList, toList)
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

class (Parsable v, Hashable v, Eq v, Show v, Serialize v) => Value v where

  -- | Is this value, in a vacuum, acceptable, for some application-specific definition of acceptable?
  --   It may be useful to the programmer to note that, as the valid function may wish to look at quorums and such,
  --    it's useful to demand that inputs have Contains_1a, which means they have to be at least a Verified Recursive_1a.
  --   This is why `valid` is called in `receive`, rather than in `parse`.
  valid :: (Parsable (Recursive_1a v), Contains_Value a v, Contains_1a a v) => a -> Bool


  -- | Does this set of entities contain conflicitng values?
  conflicts :: (Parsable (Recursive_1a v), Contains_Value a v, Contains_1a a v, Hashable a, Eq a) => (HashSet a) -> Bool

  -- | The state of a Participant maintains a set of known received Recursive_1bs.
  --   Each time it is saved, this is run.
  --   If there is ever a message we can just forget about, we should do that here.
  garbage_collect :: (HashSet (Verified (Recursive_1b v))) -> (HashSet (Verified (Recursive_1b v)))

instance Value Slot_Value where
  -- | For now, everything is acceptable.
  valid _ = True
  -- | In this case, do any two of them have the same value_slot and observers?
  --   Are there the same number of values in this set as there are distinct slot numbers, for each different observer value?
  conflicts x ={-# SCC conflicts #-} any (\s -> ((length s) /= (length (HashSet.map (value_slot . extract_value) s)))) $
                                         map (\y -> fromList $ filter (((extract_observer_quorums y) ==) . extract_observer_quorums) (toList x)) (toList x)
  -- | TODO: figure out what should even be done here
  garbage_collect = id


-- | Encode a Proposal_1a to a bytestring using Thrift
instance {-# OVERLAPPING #-} Encodable Proposal_1a where
  encode = encode_Slot_Value (BinaryProtocol EmptyTransport)
