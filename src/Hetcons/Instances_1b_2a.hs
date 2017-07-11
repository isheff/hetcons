{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Instances_1b_2a (well_formed_2a) where


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
                      ,Hetcons_Exception_Invalid_Phase_1b
                      ,Hetcons_Exception_Invalid_Phase_2a)
    )
import Hetcons.Instances_1a ()
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
       ,recursive_1b_non_recursive
       ,recursive_1b_proposal
       ,recursive_1b_conflicting_phase2as
    , Recursive_2a (Recursive_2a )
    , Recursive_2b (Recursive_2b )
    , Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
    , Parsable
       ,parse
    )
import Hetcons.Value (conflicts)

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
                     ,participant_ID_crypto_id
                     )

import           Crypto.Hash.Algorithms (SHA224(SHA224)
                                        ,SHA256(SHA256)
                                        ,SHA384(SHA384)
                                        ,SHA512(SHA512))
import           Control.Monad          (liftM, liftM2, mapM_)
import           Control.Monad.Except   (throwError, catchError, MonadError)
import           Crypto.Random          (DRG)
import           Data.ByteString.Lazy   (ByteString, unpack)
import           Data.Either.Combinators(mapLeft)
import           Data.Foldable          (null
                                        ,length
                                        ,maximumBy)
import           GHC.Generics           (Generic)
import           Data.Hashable          (Hashable
                                        ,hashWithSalt)
import           Data.HashMap.Strict    (elems)
import           Data.HashSet           (HashSet
                                        ,insert
                                        ,intersection
                                        ,unions
                                        ,toList
                                        ,fromList
                                        ,singleton)
import qualified Data.HashSet as HashSet(map)
import           Data.List              (head)
import           Data.Maybe             (catMaybes)
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
import           Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import           Thrift.Transport.Empty (EmptyTransport(EmptyTransport))




-- | Phase_1b s carry 1a and 2a messages with them.
-- | Recursive_1b s carry parsed and verified versions of these.
instance Hashable Recursive_1b where
  hashWithSalt s x = hashWithSalt s ((non_recursive x) :: Phase_1b)

instance Recursive Phase_1b Recursive_1b where
  non_recursive = recursive_1b_non_recursive


-- | Phase_2a s carry phase 1b messages with them.
-- | Recursive_2a s carry parsed and verified versions of these.
instance Hashable Recursive_2a where
  hashWithSalt s (Recursive_2a x) = hashWithSalt s x
instance Recursive Phase_2a Recursive_2a where
  non_recursive (Recursive_2a x) = default_Phase_2a {phase_2a_phase_1bs = HashSet.map signed x}



well_formed_1b :: (MonadError Hetcons_Exception m) => Recursive_1b -> m ()
well_formed_1b (Recursive_1b {
                  recursive_1b_non_recursive = non_recursive
                 ,recursive_1b_proposal = proposal
                 ,recursive_1b_conflicting_phase2as = conflicting_phase2as})
  = mapM_ (\x -> if (extract_observer_quorums proposal) /= (extract_observer_quorums x)
                then throwError $ Hetcons_Exception_Invalid_Phase_1b (default_Invalid_Phase_1b {
                       invalid_Phase_1b_offending_phase_1b = non_recursive
                       ,invalid_Phase_1b_explanation = Just $ pack "not all contained phase_2as had the same quorums as this phase_1b"
                       })
                else if not $ conflicts $ fromList [extract_value proposal, extract_value x]
                        then throwError $ Hetcons_Exception_Invalid_Phase_1b (default_Invalid_Phase_1b {
                               invalid_Phase_1b_offending_phase_1b = non_recursive
                               ,invalid_Phase_1b_explanation = Just $ pack "not all contained phase_2as conflict with the proposal"
                               })
                        else return ())
      $ toList conflicting_phase2as



-- | For a 1b object, we verify the proposal and 2a messages it carries, and parse the original message
-- | We're also going to verify the 1b's well-formedness, because that has to happen somewhere.
instance {-# OVERLAPPING #-} Parsable Recursive_1b where
  parse payload =
    do { non_recursive <- parse payload -- (Either Hetcons_Exception) Monad
       ; proposal <- verify $ phase_1b_proposal non_recursive
       ; conflicting_phase2as <- mapM verify $ toList $ phase_1b_conflicting_phase2as non_recursive
       ; let r1b = Recursive_1b {
                      recursive_1b_non_recursive = non_recursive
                     ,recursive_1b_proposal = proposal
                     ,recursive_1b_conflicting_phase2as = fromList conflicting_phase2as}
       ; well_formed_1b r1b
       ; return r1b}

well_formed_2a :: (MonadError Hetcons_Exception m) => Recursive_2a -> m ()
well_formed_2a r2a@(Recursive_2a s) =
  do { if 1 /= (length $ HashSet.map extract_value s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "there were 1bs with different values in this 2a, or no 1bs at all"})
          else return ()
     ; if 1 /= (length $ HashSet.map extract_observer_quorums s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "there were 1bs with different observers in this 2a"})
          else return ()
     ; let observers = extract_observer_quorums r2a
     ; if 0 == length observers
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "at this time, we require that observer quorums be listed by participant ID"})
          else return ()
     ; let quorums_crypto_ids = HashSet.map (HashSet.map participant_ID_crypto_id) $ unions $ elems observers
     ; let crypto_ids_of_1bs = fromList $ catMaybes $ toList $ HashSet.map (signed_Hash_crypto_id . signed_Message_signature . signed) s
     ; if all (\q -> (q /= (intersection q crypto_ids_of_1bs))) quorums_crypto_ids
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "this set of 1bs does not satisfy any known quorum"})
          else return ()
     }

-- | for a 2a message, we parse the original mesage, and verify the 1b messages it carries.
instance {-# OVERLAPPING #-} Parsable Recursive_2a where
  parse payload =
    do { non_recursive <- parse payload
       ; l_set <- mapM verify $ toList $ phase_2a_phase_1bs non_recursive
       ; let set = fromList l_set
       ; if (length (HashSet.map (recursive_1b_proposal . original) set)) > 1
            then throwError $ Hetcons_Exception_Invalid_Phase_2a default_Invalid_Phase_2a {
                                 invalid_Phase_2a_offending_phase_2a = non_recursive
                                ,invalid_Phase_2a_explanation = Just $ pack "More than 1 proposal value present."}
            else return ()
       ; well_formed_2a $ Recursive_2a set
       ; return $ Recursive_2a set}






instance {-# OVERLAPPING #-} Contains_1a Recursive_1b where
  extract_1a = extract_1a . recursive_1b_proposal


-- | The "value" carried by a 1b is actually tricky: it may be set by the 2a s carried within.
-- | This relies on having already checked that the phase_2as do indeed conflict with the given 1b
instance {-# OVERLAPPING #-} Contains_Value Recursive_1b where
  extract_value (Recursive_1b {
                   recursive_1b_conflicting_phase2as = phase_2as
                  ,recursive_1b_proposal = proposal})
    = if null phase_2as
         then extract_value proposal
         else extract_value $ maximumBy (\x y -> compare (extract_ballot x) (extract_ballot y)) phase_2as


instance {-# OVERLAPPING #-} Contains_1bs (Recursive_1b) where
  extract_1bs (Recursive_1b {recursive_1b_conflicting_phase2as = phase_2as}) = unions $ map extract_1bs $ toList phase_2as

instance {-# OVERLAPPING #-} Contains_1bs (Verified (Recursive_1b)) where
  extract_1bs b = insert b $ extract_1bs b



instance {-# OVERLAPPING #-} Contains_1a Recursive_2a where
  extract_1a (Recursive_2a x) = extract_1a $ head $ toList x

-- | The "value" carried by a 2a is actually tricky:
-- | This relies on this 2a already having been verified to ensure that, for instance, all 1bs within have the same value
instance {-# OVERLAPPING #-} Contains_Value Recursive_2a where
  extract_value (Recursive_2a x) = extract_value $ head $ toList x

instance {-# OVERLAPPING #-} Contains_1bs (Recursive_2a) where
  extract_1bs (Recursive_2a x) = x
