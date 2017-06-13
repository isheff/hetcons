{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Instances_Proof_of_Consensus (observers_proven) where

import Hetcons.Contains_Value (
      Contains_Value
        ,extract_value
    , Contains_1a
        ,extract_1a
        ,extract_observer_quorums
    , Ballot
        ,extract_ballot
    ,Contains_1bs
        ,extract_1bs
    )
import Hetcons.Hetcons_Exception (
     Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                      ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                      ,Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                      ,Hetcons_Exception_Invalid_Signed_Hash
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                      ,Hetcons_Exception_Unparsable_Hashable_Message
                      ,Hetcons_Exception_Invalid_Proof_of_Consensus)
    )
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Quorums (verify_quorums)
import Hetcons.Signed_Message
    ( verify
    , sign
    , Verified() -- Note that we do not export any constructors for Verified. The only way data should end up in this type is if it's passed through the Verify function.
       ,original
       ,signed
    , Recursive
       ,non_recursive
    , Recursive_1a(Recursive_1a)
       ,recursive_1a_non_recursive
       ,recursive_1a_filled_in
    , Recursive_1b(Recursive_1b)
       ,recursive_1b_proposal
       ,recursive_1b_conflicting_phase2as
    , Recursive_2a (Recursive_2a )
    , Recursive_2b (Recursive_2b )
    , Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
    , Parsable
       ,parse
    )
import Hetcons.Quorums (verify_quorums)

import Hetcons_Consts(sUPPORTED_HASH_SHA2_DESCRIPTOR
                     ,sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR)
import Hetcons_Types (Signed_Message (Signed_Message)
                        ,signed_Message_payload
                        ,signed_Message_signature
                        ,default_Signed_Message
                     ,Signed_Hash (Signed_Hash)
                        ,signed_Hash_signature
                        ,signed_Hash_hash_type_descriptor
                        ,signed_Hash_crypto_id
                        ,default_Signed_Hash
                     ,Hash_Type_Descriptor (Hash_Type_Descriptor)
                        ,hash_Type_Descriptor_sha2
                        ,default_Hash_Type_Descriptor
                     ,Hash_Sha2_Descriptor
                     ,Crypto_ID(Crypto_ID)
                        ,crypto_ID_public_crypto_key
                        ,default_Crypto_ID
                     ,Public_Crypto_Key(Public_Crypto_Key)
                        ,public_Crypto_Key_public_crypto_key_x509
                        ,default_Public_Crypto_Key
                     ,Public_Crypto_Key_X509
                     ,No_Supported_Hash_Sha2_Descriptor_Provided
                        ,no_Supported_Hash_Sha2_Descriptor_Provided_offending_hash_sha2_descriptor
                        ,no_Supported_Hash_Sha2_Descriptor_Provided_supported_hash_sha2_descriptor
                        ,no_Supported_Hash_Sha2_Descriptor_Provided_explanation
                        ,default_No_Supported_Hash_Sha2_Descriptor_Provided
                     ,Invalid_Signed_Hash
                        ,invalid_Signed_Hash_signed_hash
                        ,invalid_Signed_Hash_explanation
                        ,default_Invalid_Signed_Hash
                     ,Unparsable_Hashable_Message
                        ,unparsable_Hashable_Message_message
                        ,unparsable_Hashable_Message_explanation
                        ,default_Unparsable_Hashable_Message
                     ,Descriptor_Does_Not_Match_Public_Crypto_Key
                        ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key_type_descriptor
                        ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key
                        ,descriptor_Does_Not_Match_Public_Crypto_Key_explanation
                        ,default_Descriptor_Does_Not_Match_Public_Crypto_Key
                     ,Descriptor_Does_Not_Match_Crypto_ID
                        ,descriptor_Does_Not_Match_Crypto_ID_crypto_id_type_descriptor
                        ,descriptor_Does_Not_Match_Crypto_ID_crypto_id
                        ,descriptor_Does_Not_Match_Crypto_ID_explanation
                        ,default_Descriptor_Does_Not_Match_Crypto_ID
                     ,Descriptor_Does_Not_Match_Signed_Hash
                        ,descriptor_Does_Not_Match_Signed_Hash_signed_hash_type_descriptor
                        ,descriptor_Does_Not_Match_Signed_Hash_signed_hash
                        ,descriptor_Does_Not_Match_Signed_Hash_explanation
                        ,default_Descriptor_Does_Not_Match_Signed_Hash
                     ,No_Supported_Hash_Type_Descriptor_Provided
                        ,no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor
                        ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor
                        ,no_Supported_Hash_Type_Descriptor_Provided_explanation
                        ,default_No_Supported_Hash_Type_Descriptor_Provided
                     ,Descriptor_Does_Not_Match_Signed_Hash
                        ,descriptor_Does_Not_Match_Signed_Hash_signed_hash_type_descriptor
                        ,descriptor_Does_Not_Match_Signed_Hash_signed_hash
                        ,descriptor_Does_Not_Match_Signed_Hash_explanation
                        ,default_Descriptor_Does_Not_Match_Signed_Hash
                     ,Signed_Hash_Type_Descriptor(Signed_Hash_Type_Descriptor)
                        ,signed_Hash_Type_Descriptor_hash_type_descriptor
                        ,signed_Hash_Type_Descriptor_crypto_id
                        ,default_Signed_Hash_Type_Descriptor
                     ,Crypto_ID_Type_Descriptor(Crypto_ID_Type_Descriptor)
                        ,crypto_ID_Type_Descriptor_public_crypto_key
                        ,default_Crypto_ID_Type_Descriptor
                     ,Public_Crypto_Key_Type_Descriptor(Public_Crypto_Key_Type_Descriptor)
                        ,public_Crypto_Key_Type_Descriptor_public_crypto_key_x509
                        ,default_Public_Crypto_Key_Type_Descriptor
                     ,No_Supported_Crypto_ID_Type_Descriptor_Provided
                        ,no_Supported_Crypto_ID_Type_Descriptor_Provided_offending_crypto_id_type_descriptor
                        ,no_Supported_Crypto_ID_Type_Descriptor_Provided_supported_crypto_id_type_descriptor
                        ,no_Supported_Crypto_ID_Type_Descriptor_Provided_explanation
                        ,default_No_Supported_Crypto_ID_Type_Descriptor_Provided
                     ,Invalid_Phase_1b
                        ,invalid_Phase_1b_offending_phase_1b
                        ,invalid_Phase_1b_explanation
                        ,default_Invalid_Phase_1b
                     ,Invalid_Phase_2a
                        ,invalid_Phase_2a_offending_phase_2a
                        ,invalid_Phase_2a_explanation
                        ,default_Invalid_Phase_2a
                     ,Invalid_Phase_2b
                        ,invalid_Phase_2b_offending_phase_2b
                        ,invalid_Phase_2b_explanation
                        ,default_Invalid_Phase_2b
                     ,Invalid_Proof_of_Consensus
                        ,invalid_Proof_of_Consensus_offending_proof_of_consensus
                        ,invalid_Proof_of_Consensus_explanation
                        ,default_Invalid_Proof_of_Consensus
                     ,Proposal_1a
                        ,proposal_1a_observers
                        ,encode_Proposal_1a
                        ,decode_Proposal_1a
                     ,Phase_1b
                        ,phase_1b_proposal
                        ,phase_1b_conflicting_phase2as
                        ,encode_Phase_1b
                        ,decode_Phase_1b
                     ,Phase_2a
                        ,phase_2a_phase_1bs
                        ,default_Phase_2a
                        ,encode_Phase_2a
                        ,decode_Phase_2a
                     ,Phase_2b
                        ,phase_2b_phase_1bs
                        ,default_Phase_2b
                        ,encode_Phase_2b
                        ,decode_Phase_2b
                     ,Proof_of_Consensus
                        ,proof_of_Consensus_phase_2bs
                        ,default_Proof_of_Consensus
                        ,encode_Proof_of_Consensus
                        ,decode_Proof_of_Consensus
                     ,Participant_ID
                        ,participant_ID_crypto_id
                     )

import           Crypto.Hash.Algorithms (SHA224(SHA224)
                                        ,SHA256(SHA256)
                                        ,SHA384(SHA384)
                                        ,SHA512(SHA512))
import           Control.Monad          (liftM, liftM2, mapM_)
import           Control.Monad.Except   (throwError)
import           Crypto.Random          (DRG)
import           Data.ByteString.Lazy   (ByteString, unpack)
import           Data.Either.Combinators(mapLeft)
import           Data.Foldable          (null
                                        ,any
                                        ,length
                                        ,maximum)
import           GHC.Generics           (Generic)
import           Data.Hashable          (Hashable
                                        ,hashWithSalt)
import           Data.HashMap.Strict    (keys, (!))
import           Data.HashSet           (HashSet
                                        ,member
                                        ,unions
                                        ,intersection
                                        ,toList
                                        ,fromList
                                        ,singleton)
import qualified Data.HashSet as HashSet(map, empty, foldr, filter)
import           Data.List              (head)
import           Data.Typeable          (Typeable )
import           Data.Serialize         (Serialize
                                        ,get
                                        ,put
                                        ,encodeLazy
                                        ,decodeLazy)
import           Data.Serialize.Get     (remaining, getLazyByteString)
import           Data.Serialize.Put     (putWord8)
import           Data.Text.Lazy         (pack)
import           Data.Traversable       (mapM)
import qualified EasyX509       as X509 (sign
                                        ,verify
                                        ,Signer)
import           Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import           Thrift.Transport.Empty (EmptyTransport(EmptyTransport))


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
