{-# LANGUAGE FlexibleContexts #-}

-- | A module for some utility functions concerning extracting stuff from messages
module Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
    , Contains_1a
        ,extract_1a
        ,extract_observer_quorums
    , Ballot
        ,extract_ballot
    ,Contains_1bs
        ,extract_1bs
    ) where

import Hetcons.Signed_Message
    ( Verified
     ,Recursive_1b
     ,Recursive_1a(recursive_1a_filled_in)
     ,Parsable
     ,signed
     ,original )

import Hetcons_Types
    ( Observers(Observers
       ,observers_observer_quorums)
     ,Signed_Message(signed_Message_signature)
     ,Proposal_1a(Proposal_1a
       ,proposal_1a_observers
       ,proposal_1a_timestamp)
     ,Value
     ,Signed_Hash(signed_Hash_signature)
     ,Participant_ID )

import Data.ByteString.Lazy ( ByteString )
import Data.HashMap.Strict ( HashMap )
import Data.HashSet ( HashSet )
import Data.Int ( Int64 )

-- | Messages which are part of a ballot of consensus contain a 1A message which kicked off that ballot.
class Contains_1a a v where
  -- | The 1a message that originated the Ballot of which this message is a part.
  extract_1a :: a -> (Verified (Recursive_1a v))

instance {-# OVERLAPPABLE #-} (Parsable a, Contains_1a a v) => Contains_1a (Verified a) v where
  extract_1a = extract_1a . original


-- | a ballot "number" is an Int64, representing a timestamp, and a bytestring, representing a hashed value.
--   These are, notably, orderable.
type Ballot = (Int64, ByteString)
extract_ballot :: (Parsable (Recursive_1a v), Contains_1a a v) => a -> Ballot
extract_ballot v = let proposal = extract_1a v
                       in (proposal_1a_timestamp $ recursive_1a_filled_in $ original proposal,
                           signed_Hash_signature $ signed_Message_signature $ signed proposal)

-- | What are the quorums in the consensus of this Message?
--   Specifically, for each observer, returns a set of sets of participants which represent quorums.
extract_observer_quorums :: (Parsable (Recursive_1a v)) => (Contains_1a a v) => a -> (HashMap Participant_ID (HashSet (HashSet Participant_ID)))
extract_observer_quorums x = let (Proposal_1a{proposal_1a_observers=Just Observers{observers_observer_quorums=Just y}})= recursive_1a_filled_in $ original $ extract_1a x
                              in y



-- | Each message in consensus carries some kind of value, but this may not be the value carried by the 1A that kicked off that Ballot.
--   For instance, 1Bs may carry 2As representing that a participant has "agreed" to some previous value.
class Contains_Value a v where
  -- | The value carried by this message
  extract_value :: a -> v

-- | By default, the value a message "contains" is that carried by the 1A that kicked off its ballot.
instance {-# OVERLAPPABLE #-} (Parsable a, Contains_Value a v) => Contains_Value (Verified a) v where
  extract_value = extract_value . original

-- | A Value contains a Value: itself.
instance {-# OVERLAPPING #-} Contains_Value v where
  extract_value = id


-- | Some messages, like 2as, carry 1bs within them. Sometimes it's useful to get a set of all the 1bs within.
class Contains_1bs a v where
  -- | the set of 1bs within a given message
  extract_1bs :: a -> (HashSet (Verified (Recursive_1b v)))

-- | when a type contains 1Bs, so does the Verified version of that type.
instance {-# OVERLAPPABLE #-} (Parsable a, Contains_1bs a v) => Contains_1bs (Verified a) v where
  extract_1bs = extract_1bs . original

