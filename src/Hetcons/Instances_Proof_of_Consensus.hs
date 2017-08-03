{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Instances_Proof_of_Consensus (observers_proven) where


import Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
     ,Contains_1a
        ,extract_1a
        ,extract_observer_quorums
     ,Ballot
        ,extract_ballot
     ,Contains_1bs
        ,extract_1bs
    )
import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_Invalid_Proof_of_Consensus) )
import Hetcons.Instances_2b ()
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,Verified
     ,Parsable
       ,parse
     ,Recursive
       ,non_recursive
     ,Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
     ,Recursive_2b (Recursive_2b )
     ,Monad_Verify(verify)
     ,signed
     ,original )

import Hetcons_Types
    ( Participant_ID(participant_ID_crypto_id)
     ,Invalid_Proof_of_Consensus(invalid_Proof_of_Consensus_explanation
                                ,invalid_Proof_of_Consensus_offending_proof_of_consensus)
     ,Signed_Hash(signed_Hash_crypto_id)
     ,Signed_Message(signed_Message_signature)
     ,Proof_of_Consensus(
        proof_of_Consensus_phase_2bs)
       ,default_Proof_of_Consensus
       ,default_Invalid_Proof_of_Consensus
       ,encode_Proof_of_Consensus )

import Control.Monad.Except ( throwError )
import Data.Foldable ( Foldable(length), any )
import Data.HashMap.Strict ( keys, (!) )
import Data.HashSet
    ( HashSet, unions, toList, member, intersection, fromList )
import qualified Data.HashSet as HashSet ( map, filter )
import Data.List ( head )
import Data.Traversable ( mapM )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

instance {-# OVERLAPPING #-} Encodable Proof_of_Consensus where
  encode = encode_Proof_of_Consensus (BinaryProtocol EmptyTransport)

-- | Proof_of_Consensus messages carry signed 2b messages with them.
-- | Recursive_Proof_of_Consensus objects carry parsed and verified versions of these.
instance Recursive Proof_of_Consensus Recursive_Proof_of_Consensus where
  non_recursive (Recursive_Proof_of_Consensus x) = default_Proof_of_Consensus {proof_of_Consensus_phase_2bs = HashSet.map signed x}

class Observers_Provable a where
  -- | does thers exist at least one observer such that:
  -- |   there exists a quorum (according to that observer) such that:
  -- |     each participant in that quorum received 1bs from the same quorum (according to that observer)
  observers_proven :: a -> (HashSet Participant_ID)

instance Observers_Provable Recursive_Proof_of_Consensus where
  observers_proven rpoc@(Recursive_Proof_of_Consensus set) =
    let observers = extract_observer_quorums rpoc
        -- given a quorum (set) of Participant_IDs, filters the given set of Verified anythings for only those elements signed by a quorum member
        filter_by_quorum q = HashSet.filter (\x -> case (signed_Hash_crypto_id $ signed_Message_signature $ signed x) of
                                                     Just y -> (member y (HashSet.map participant_ID_crypto_id q))
                                                     Nothing -> False)
       -- Given a set of 2bs, returns the largest set of 1bs which every one of the 2b senders has received
        quorum_of_1bs quorum_of_2bs = let (x:xs) = toList $ HashSet.map ((\(Recursive_2b x) -> x) . original) quorum_of_2bs
                                       in foldr intersection x xs
        -- Given an observer x and a quorum of Participant_IDs q, returns whether any quorum of x's is satisfied by a set of 1bs which everyone in q has received.
        is_proven_with_quorum_of_2bs x q = any (\x_quorum -> ((length x_quorum) == (length (filter_by_quorum x_quorum (quorum_of_1bs q))))) $ observers!x
        -- has the observer x achieved consensus given this proof?
        is_proven x = any (\x_quorum -> let q2bs = filter_by_quorum x_quorum set -- is there any quorum of x's such that the 2bs from that quorum
                                             -- have the whole quorum, and all feature a quorum of 1bs
                                         in (((length x_quorum) == (length q2bs)) && (is_proven_with_quorum_of_2bs x q2bs)))
                          $ observers!x
     in fromList $ filter is_proven $ keys observers -- which observers have achieved consensus?

instance Observers_Provable (HashSet (Verified Recursive_2b)) where
  observers_proven = observers_proven . Recursive_Proof_of_Consensus

instance (Parsable a, Observers_Provable a) => Observers_Provable (Verified a) where
  observers_proven = observers_proven . original

-- | For a Proof_of_Consensus message, we parse the original message, and verify the 2b messages it carries.
instance {-# OVERLAPPING #-} Parsable Recursive_Proof_of_Consensus where
  parse payload =
    do { non_recursive <- parse payload
       ; l_set <- mapM verify $ toList $ proof_of_Consensus_phase_2bs non_recursive
       ; let set = fromList l_set
       ; if (length (HashSet.map extract_1a set)) > 1
            then throwError $ Hetcons_Exception_Invalid_Proof_of_Consensus default_Invalid_Proof_of_Consensus {
                                 invalid_Proof_of_Consensus_offending_proof_of_consensus = non_recursive
                                ,invalid_Proof_of_Consensus_explanation =
                                  Just "More than 1 proposal_1a present. A proof should be assembled using the results initiated by a single proposal."}
            else return ()
       ; if (length (HashSet.map extract_value set)) > 1
            then throwError $ Hetcons_Exception_Invalid_Proof_of_Consensus default_Invalid_Proof_of_Consensus {
                                 invalid_Proof_of_Consensus_offending_proof_of_consensus = non_recursive
                                ,invalid_Proof_of_Consensus_explanation = Just "More than 1 value present. We must prove consensus on a single value."}
            else return ()
       ; if (0 == (length (observers_proven set)))
            then throwError $ Hetcons_Exception_Invalid_Proof_of_Consensus default_Invalid_Proof_of_Consensus {
                                 invalid_Proof_of_Consensus_offending_proof_of_consensus = non_recursive
                                ,invalid_Proof_of_Consensus_explanation = Just "this so-called proof does not prove consensus for any observer"}
            else return $ Recursive_Proof_of_Consensus set
       }


instance {-# OVERLAPPING #-} Contains_1a Recursive_Proof_of_Consensus where
  extract_1a (Recursive_Proof_of_Consensus x) = extract_1a $ head $ toList x
instance {-# OVERLAPPING #-} Contains_Value Recursive_Proof_of_Consensus where
  extract_value (Recursive_Proof_of_Consensus x) = extract_value $ head $ toList x
instance {-# OVERLAPPING #-} Contains_1bs (Recursive_Proof_of_Consensus) where
  extract_1bs (Recursive_Proof_of_Consensus x) = unions $ map extract_1bs $ toList x
