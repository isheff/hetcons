{-# LANGUAGE UndecidableInstances #-}
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
    , Contains_Quorums
    , Ballot
    , Contains_Ballot
       ,extract_ballot
    , Contains_Value
       ,extract_value
    , Contains_1bs
       ,extract_1bs
    ) where

import Hetcons.Hetcons_Exception(Hetcons_Exception)
import Hetcons.Parsable (Parsable)
import Hetcons.Quorums (Monad_Verify_Quorums)
import Hetcons.Signed_Message (Recursive_1a
                                 ,recursive_1a_value
                                 ,recursive_1a_filled_in
                                 ,recursive_1a_non_recursive
                              ,Recursive_1b
                              ,Recursive_2a
                              ,Recursive_2b
                              ,Recursive_Proof_of_Consensus
                              ,Monad_Verify
                              ,Verified
                                ,original
                                ,signed
                              ,Encodable
                                 ,encode
                              )

import Charlotte_Types  (Slot_Value
                         ,slot_Value_slot
                         ,encode_Slot_Value
                      ,Value_Witness
                      ,Participant_ID
                      ,Proposal_1a(Proposal_1a)
                         ,proposal_1a_observers
                         ,proposal_1a_timestamp
                         ,observers_observer_quorums
                      ,Observers(Observers)
                         ,observers_observer_quorums
                      ,signed_Hash_signature
                      ,signed_Message_signature
                      ,hetcons_Message_proposals
                      ,hetcons_Message_phase_1as
                      ,signed_Index_signature
                      )

import Control.Monad.Except ( MonadError )
import Control.Monad.Logger (MonadLogger)
import Data.ByteString.Lazy ( ByteString )
import Data.Foldable (length, toList)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy ( HashMap )
import Data.Int (Int64)
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashSet (HashSet, fromList)
import Data.Vector (elemIndex, (!))
import Thrift.Protocol.Compact ( CompactProtocol(CompactProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

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
  value_valid :: ( Monad_Verify_Quorums m
                 , MonadLogger m
                 , MonadError Hetcons_Exception m
                 , Monad_Verify (Recursive_1a v) m
                 , Monad_Verify (Recursive_1b v) m
                 , Monad_Verify (Recursive_2a v) m
                 , Monad_Verify (Recursive_2b v) m
                 , Monad_Verify (Recursive_Proof_of_Consensus v) m) => Value_Witness -> v -> (m Bool)

  -- | Does this set of entities contain conflicitng values?
  --   In this case, do any two of them have the same value_slot and observers?
  value_conflicts :: (Foldable f) => (f (Verified (Recursive_1a v))) -> Bool

  -- | The state of a Participant maintains a set of known received Recursive_1bs.
  --   Each time it is saved, this is run.
  --   If there is ever a message we can just forget about, we should do that here.
  --   TODO: figure out what should even be done here
  garbage_collect :: HashSet (Verified (Recursive_1b v)) -> HashSet (Verified (Recursive_1b v))


valid :: forall a v m. ( Monad_Verify_Quorums m
                       , MonadLogger m
                       , MonadError Hetcons_Exception m
                       , Monad_Verify (Recursive_1a v) m
                       , Monad_Verify (Recursive_1b v) m
                       , Monad_Verify (Recursive_2a v) m
                       , Monad_Verify (Recursive_2b v) m
                       , Monad_Verify (Recursive_Proof_of_Consensus v) m
                       , Value v
                       , Contains_Value (Verified (a v)) v) => Value_Witness -> (Verified (a v)) -> m Bool
valid witness = ((value_valid witness) :: v -> m Bool) . (extract_value  :: (Verified (a v)) -> v)
-- valid _ = True -- For now, everything is acceptable.

class Conflictable a where
  conflicts :: a -> Bool

instance {-# OVERLAPPING #-} forall a v . (Value v, Contains_1a (Verified (a v)) v,  Hashable (Verified (a v)), Eq (Verified (a v))) => Conflictable (HashSet (Verified (a v))) where
  conflicts = value_conflicts . (HashSet.map (extract_1a :: (Verified (a v)) -> (Verified (Recursive_1a v))))

instance {-# OVERLAPPABLE #-} forall a v . (Value v, Contains_1a (a v) v,  Hashable (a v), Eq (a v)) => Conflictable (HashSet (a v)) where
  conflicts = value_conflicts . (HashSet.map (extract_1a :: (a v) -> (Verified (Recursive_1a v))))



instance Value Slot_Value where
  value_valid _ _ = return True
-- Are there the same number of values in this set as there are distinct slot numbers, for each different observer value?
  value_conflicts x =
    any (\s -> ((length s) /= (length (HashSet.map (slot_Value_slot . recursive_1a_value . original) s)))) $
        map (\y -> fromList $ filter (((proposal_1a_observers $ recursive_1a_filled_in $ original  y) ==) . (proposal_1a_observers . recursive_1a_filled_in . original))
                                     (toList x))
            (toList x)
  garbage_collect = id

-- | Encode a Proposal_1a to a bytestring using Thrift
instance {-# OVERLAPPING #-} Encodable Slot_Value where
  encode = encode_Slot_Value (CompactProtocol EmptyTransport)



instance {-# OVERLAPPABLE #-} (Value v, Contains_1a a v) => Contains_1a (Verified a) v where
  extract_1a = extract_1a . original

instance {-# OVERLAPPABLE #-} (Value v, Contains_Value a v) => Contains_Value (Verified a) v where
  extract_value = extract_value . original


-- | a ballot "number" is an Int64, representing a timestamp, and a bytestring, representing a hashed value.
--   These are, notably, orderable.
type Ballot = (Int64, ByteString)

class Contains_Ballot a where
  extract_ballot :: a -> Ballot

instance {-# OVERLAPPABLE #-} forall a v . (Value v, Contains_1a (a v) v) => Contains_Ballot (Verified (a v)) where
  extract_ballot x = extract_ballot ((extract_1a x) :: Verified (Recursive_1a v))

instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Ballot (Verified (Recursive_1a v)) where
  extract_ballot proposal =(proposal_1a_timestamp $ recursive_1a_filled_in $ original proposal,
                            -- in a Hetcons_Message, the signed proposal should be one of the proposals in a list, so we find that one
                            case elemIndex (recursive_1a_non_recursive $ original proposal) (hetcons_Message_proposals $ signed proposal) of 
                                 Nothing -> error ("this 1A appears not to contain its own proposal in its signed version")
                                 -- and then use the signature listed for that proposal as the less significant ballot entry
                                 Just i -> signed_Hash_signature $ signed_Index_signature $ (hetcons_Message_phase_1as $ signed proposal)!(fromIntegral i))

class Contains_Quorums a where
  extract_observer_quorums :: a -> (HashMap Participant_ID (HashSet (HashSet Participant_ID)))
-- | What are the quorums in the consensus of this Message?
--   Specifically, for each observer, returns a set of sets of participants which represent quorums.
instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Quorums (Verified (Recursive_1a v)) where
  extract_observer_quorums x = let (Proposal_1a{proposal_1a_observers=Just Observers{observers_observer_quorums=Just y}})= recursive_1a_filled_in $ original x
                                in y

instance {-# OVERLAPPABLE #-} forall a v . (Value v, Contains_1a (a v) v) => Contains_Quorums (Verified (a v)) where
  extract_observer_quorums = extract_observer_quorums . (extract_1a :: (Verified (a v)) -> (Verified (Recursive_1a v)))





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

