{-# LANGUAGE FlexibleInstances #-}

module Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
    , Contains_1a
        ,extract_1a
        ,extract_observer_quorums
    , Ballot
        ,extract_ballot
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
import Data.Int (Int64)
import Data.List (head)
import Data.Ord (compare)

class Contains_1a a where
  extract_1a :: a -> (Verified (Recursive_1a))

instance {-# OVERLAPPABLE #-} (Parsable a, Contains_1a a) => Contains_1a (Verified a) where
  extract_1a = extract_1a . original
instance {-# OVERLAPPING #-} Contains_1a (Verified Recursive_1a) where
  extract_1a = id
instance {-# OVERLAPPING #-} Contains_1a Recursive_1b where
  extract_1a = extract_1a . recursive_1b_proposal
instance {-# OVERLAPPING #-} Contains_1a Recursive_2a where
  extract_1a (Recursive_2a x) = extract_1a $ head $ toList x
instance {-# OVERLAPPING #-} Contains_1a Recursive_2b where
  extract_1a (Recursive_2b x) = extract_1a $ head $ toList x
instance {-# OVERLAPPING #-} Contains_1a Recursive_Proof_of_Consensus where
  extract_1a (Recursive_Proof_of_Consensus x) = extract_1a $ head $ toList x


type Ballot = (Int64, ByteString)
extract_ballot :: (Contains_1a a) => a -> Ballot
extract_ballot v = let proposal = extract_1a v
                       in (proposal_1a_timestamp $ recursive_1a_filled_in $ original proposal,
                           signed_Hash_signature $ signed_Message_signature $ signed proposal)

extract_observer_quorums' :: Proposal_1a -> (HashMap Participant_ID (HashSet (HashSet Participant_ID)))
extract_observer_quorums' (Proposal_1a {proposal_1a_observers = Just Observers {observers_observer_quorums = Just x}}) = x
extract_observer_quorums :: (Contains_1a a) => a -> (HashMap Participant_ID (HashSet (HashSet Participant_ID)))
extract_observer_quorums = extract_observer_quorums' . recursive_1a_filled_in . original . extract_1a



-- TODO: I've noticed that value contained is subjective. It matters which observer is watching. For instance, a 1b may carry multiple 2as, which carry entirely different values, with entirely disjoint quorums, should observers have disjoint quorums

class Contains_Value a where
  extract_value :: a -> Value

instance {-# OVERLAPPABLE #-} (Parsable a, Contains_Value a) => Contains_Value (Verified a) where
  extract_value = extract_value . original

instance {-# OVERLAPPING #-} Contains_Value Value where
  extract_value = id
instance {-# OVERLAPPING #-} Contains_Value Proposal_1a where
  extract_value = proposal_1a_value
instance {-# OVERLAPPING #-} Contains_Value Recursive_1a where
  extract_value = extract_value . recursive_1a_filled_in

-- | The "value" carried by a 1b is actually tricky: it may be set by the 2a s carried within.
-- | This relies on having already checked that the phase_2as do indeed conflict with the given 1b
instance {-# OVERLAPPING #-} Contains_Value Recursive_1b where
  extract_value (Recursive_1b {
                   recursive_1b_conflicting_phase2as = phase_2as
                  ,recursive_1b_proposal = proposal})
    = if null phase_2as
         then extract_value proposal
         else extract_value $ maximumBy (\x y -> compare (extract_ballot x) (extract_ballot y)) phase_2as

-- | The "value" carried by a 2a is actually tricky:
-- | This relies on this 2a already having been verified to ensure that, for instance, all 1bs within have the same value
instance {-# OVERLAPPING #-} Contains_Value Recursive_2a where
  extract_value (Recursive_2a x) = extract_value $ head $ toList x


{--
instance {-# OVERLAPPING #-} Contains_Value Recursive_2b where
  extract_value (Recursive_2b x) = extract_value $ Recursive_2a x
instance {-# OVERLAPPING #-} Contains_Value Recursive_Proof_of_Consensus where
  extract_value (Recursive_Proof_of_Consensus x) = extract_value $ head $ toList x

--}
