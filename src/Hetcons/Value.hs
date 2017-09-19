{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Defines the two properties of a Value, a datatype defined in Thrift.
module Hetcons.Value
    ( Value
       ,value_valid
       ,value_conflicts
       ,garbage_collect
    , valid
    , conflicts
    , Contains_1a
       ,extract_1a
       ,extract_observer_quorums
    , Ballot
       ,extract_ballot
    , Contains_Value
       ,extract_value
    , Contains_1bs
       ,extract_1bs
    ) where

import Hetcons.Parsable (Parsable)
import Hetcons.Signed_Message (Recursive_1a
                                 ,recursive_1a_value
                                 ,recursive_1a_filled_in
                              ,Recursive_1b
                              ,Verified
                                ,original
                                ,signed
                              )

import Hetcons_Types  (Slot_Value
                         ,slot_Value_slot
                      ,Participant_ID
                      ,Proposal_1a(Proposal_1a)
                         ,proposal_1a_observers
                         ,proposal_1a_timestamp
                         ,observers_observer_quorums
                      ,Observers(Observers)
                         ,observers_observer_quorums
                      ,signed_Hash_signature
                      ,signed_Message_signature
                      )

import Data.ByteString.Lazy ( ByteString )
import Data.Foldable (length, toList)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy ( HashMap )
import Data.Int (Int64)
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashSet (HashSet, fromList)

-- | Messages which are part of a ballot of consensus contain a 1A message which kicked off that ballot.
class (Value v) => Contains_1a a v where
  -- | The 1a message that originated the Ballot of which this message is a part.
  extract_1a :: a -> (Verified (Recursive_1a v))

-- | Each message in consensus carries some kind of value, but this may not be the value carried by the 1A that kicked off that Ballot.
--   For instance, 1Bs may carry 2As representing that a participant has "agreed" to some previous value.
class (Value v) => Contains_Value a v where
  -- | The value carried by this message
  extract_value :: a -> v


class Value v where
  -- | Is this value, in a vacuum, acceptable, for some application-specific definition of acceptable?
  --   It may be useful to the programmer to note that, as the valid function may wish to look at quorums and such,
  --    it's useful to demand that inputs have Contains_1a, which means they have to be at least a Verified Recursive_1a.
  --   This is why `valid` is called in `receive`, rather than in `parse`.
  value_valid :: v -> Bool

  -- | Does this set of entities contain conflicitng values?
  --   In this case, do any two of them have the same value_slot and observers?
  value_conflicts :: (Foldable f) => (f (Verified (Recursive_1a v))) -> Bool

  -- | The state of a Participant maintains a set of known received Recursive_1bs.
  --   Each time it is saved, this is run.
  --   If there is ever a message we can just forget about, we should do that here.
  --   TODO: figure out what should even be done here
  garbage_collect :: HashSet (Verified (Recursive_1b v)) -> HashSet (Verified (Recursive_1b v))


valid :: forall a v . (Value v, Contains_Value a v) => a -> Bool
valid = (value_valid :: v -> Bool) . (extract_value  :: a -> v)
-- valid _ = True -- For now, everything is acceptable.


conflicts :: forall a v . (Value v, Contains_1a a v,  Hashable a, Eq a) => (HashSet a) -> Bool
conflicts = value_conflicts . (HashSet.map (extract_1a :: a -> (Verified (Recursive_1a v))))



instance Value Slot_Value where
  value_valid _ = True
-- Are there the same number of values in this set as there are distinct slot numbers, for each different observer value?
  value_conflicts x =
    any (\s -> ((length s) /= (length (HashSet.map (slot_Value_slot . recursive_1a_value . original) s)))) $
        map (\y -> fromList $ filter (((proposal_1a_observers $ recursive_1a_filled_in $ original  y) ==) . (proposal_1a_observers . recursive_1a_filled_in . original)) (toList x))
            (toList x)
  garbage_collect = id




instance {-# OVERLAPPABLE #-} (Value v, Contains_1a a v) => Contains_1a (Verified a) v where
  extract_1a = extract_1a . original
instance {-# OVERLAPPABLE #-} (Value v, Contains_Value a v) => Contains_Value (Verified a) v where
  extract_value = extract_value . original


-- | a ballot "number" is an Int64, representing a timestamp, and a bytestring, representing a hashed value.
--   These are, notably, orderable.
type Ballot = (Int64, ByteString)
extract_ballot :: forall a v . (Value v, Contains_1a a v) => a -> Ballot
extract_ballot x = let proposal = (extract_1a x) :: Verified (Recursive_1a v)
                    in (proposal_1a_timestamp $ recursive_1a_filled_in $ original proposal,
                        signed_Hash_signature $ signed_Message_signature $ signed proposal)

-- | What are the quorums in the consensus of this Message?
--   Specifically, for each observer, returns a set of sets of participants which represent quorums.
extract_observer_quorums :: forall a v . (Value v, Contains_1a a v) => a -> (HashMap Participant_ID (HashSet (HashSet Participant_ID)))
extract_observer_quorums x = let (Proposal_1a{proposal_1a_observers=Just Observers{observers_observer_quorums=Just y}})= recursive_1a_filled_in $ original $ ((extract_1a x) :: Verified (Recursive_1a v))
                              in y





-- | A Value contains a Value: itself.
instance {-# OVERLAPPING #-} (Value v) => Contains_Value v v where
  extract_value = id


-- | Some messages, like 2as, carry 1bs within them. Sometimes it's useful to get a set of all the 1bs within.
class Contains_1bs a v where
  -- | the set of 1bs within a given message
  extract_1bs :: a -> (HashSet (Verified (Recursive_1b v)))

-- | when a type contains 1Bs, so does the Verified version of that type.
instance {-# overlappable #-} (Value v, Contains_1bs a v) => Contains_1bs (Verified a) v where
  extract_1bs = extract_1bs . original

