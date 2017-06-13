{-# LANGUAGE FlexibleInstances #-}

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

import Hetcons.Signed_Message (Recursive_1b(Recursive_1b)
                                 ,recursive_1b_proposal
                                 ,recursive_1b_conflicting_phase2as
                              ,Recursive_1a (Recursive_1a )
                                 ,recursive_1a_filled_in
                              ,Recursive_2a (Recursive_2a )
                              ,Recursive_2b (Recursive_2b )
                              ,Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
                              ,Parsable
                              ,Verified
                                 ,original
                                 ,signed
                              )

import Hetcons_Consts ()
import Hetcons_Types  (Value
                      ,Proposal_1a(Proposal_1a)
                         ,proposal_1a_value
                         ,proposal_1a_timestamp
                         ,proposal_1a_observers
                      ,Signed_Hash
                         ,signed_Hash_signature
                      ,Signed_Message
                         ,signed_Message_signature
                      ,Participant_ID
                      ,Observers(Observers)
                         ,observers_observer_quorums
                      )

import           Data.ByteString.Lazy   (ByteString)
import Data.Foldable (null, toList, maximumBy)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet, insert)
import Data.Int (Int64)
import Data.List (head)
import Data.Ord (compare)

class Contains_1a a where
  extract_1a :: a -> (Verified (Recursive_1a))

instance {-# OVERLAPPABLE #-} (Parsable a, Contains_1a a) => Contains_1a (Verified a) where
  extract_1a = extract_1a . original


type Ballot = (Int64, ByteString)
extract_ballot :: (Contains_1a a) => a -> Ballot
extract_ballot v = let proposal = extract_1a v
                       in (proposal_1a_timestamp $ recursive_1a_filled_in $ original proposal,
                           signed_Hash_signature $ signed_Message_signature $ signed proposal)

extract_observer_quorums' :: Proposal_1a -> (HashMap Participant_ID (HashSet (HashSet Participant_ID)))
extract_observer_quorums' (Proposal_1a {proposal_1a_observers = Just Observers {observers_observer_quorums = Just x}}) = x
extract_observer_quorums :: (Contains_1a a) => a -> (HashMap Participant_ID (HashSet (HashSet Participant_ID)))
extract_observer_quorums = extract_observer_quorums' . recursive_1a_filled_in . original . extract_1a



class Contains_Value a where
  extract_value :: a -> Value

instance {-# OVERLAPPABLE #-} (Parsable a, Contains_Value a) => Contains_Value (Verified a) where
  extract_value = extract_value . original

instance {-# OVERLAPPING #-} Contains_Value Value where
  extract_value = id


-- | Some messages, like 2as, carry 1bs within them. Sometimes it's useful to get a set of all the 1bs within.
class Contains_1bs a where
  -- | the set of 1bs within a given message
  extract_1bs :: a -> (HashSet (Verified (Recursive_1b)))

instance {-# OVERLAPPABLE #-} (Parsable a, Contains_1bs a) => Contains_1bs (Verified a) where
  extract_1bs = extract_1bs . original


