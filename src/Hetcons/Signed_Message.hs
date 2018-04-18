{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Details the types, classes, and functions for signing and verifying stuff, including Recursive types.
--   For each type of Thrift message, we have a Recursive type, which contains the verified versions of any messages contained in the original.
--   We also have Verified a, which is a version of a that can only be created by cryptographic verification of a Signed Message.
module Hetcons.Signed_Message
    (Encodable
       ,encode
       ,sign
    ,To_Hetcons_Message
       ,to_Hetcons_Message
    ,From_Hetcons_Message
       ,from_Hetcons_Message
    ,Monad_Verify
       ,verify
       ,verify'
       ,verify_bytestring
       ,verify_hetcons_message
    , alter_hetcons_index
    , Verified() -- Note that we do not export any constructors for Verified. The only way data should end up in this type is if it's passed through the Verify function.
       ,original
       ,signed
    , Get_Signature
       ,signature
       ,signature_1a
       ,signature_1b
       ,signature_2a
       ,signature_2b
    , signature_bytestring
    , signature_bytestring_proof_of_consensus
    , Recursive_1a(Recursive_1a)
       ,recursive_1a_non_recursive
       ,recursive_1a_filled_in
       ,recursive_1a_value
    , Recursive_1b(Recursive_1b)
       ,recursive_1b_proposal
       ,recursive_1b_conflicting_phase2as
    , Recursive_2a (Recursive_2a )
    , Recursive_2b (Recursive_2b )
    , Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
    , Parsable
       ,parse
    ) where

import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                       ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                       ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                       ,Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided
                       ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                       ,Hetcons_Exception_Invalid_Signed_Hash
                       ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                       ,Hetcons_Exception_Unparsable_Hashable_Message) )
import Hetcons.Parsable (Parsable, parse)
import Hetcons.Quorums ( Monad_Verify_Quorums )
import Hetcons.Serializable ()

import Charlotte_Consts
    ( sUPPORTED_HASH_SHA2_DESCRIPTOR
     ,sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
     ,sUPPORTED_HASH_TYPE_DESCRIPTOR
     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
     ,sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR )
import Charlotte_Types
    ( Unparsable_Hashable_Message(unparsable_Hashable_Message_explanation
                                 ,unparsable_Hashable_Message_message)
                                 ,default_Unparsable_Hashable_Message
     ,Descriptor_Does_Not_Match_Signed_Hash(descriptor_Does_Not_Match_Signed_Hash_explanation
                                           ,descriptor_Does_Not_Match_Signed_Hash_signed_hash
                                           ,descriptor_Does_Not_Match_Signed_Hash_signed_hash_type_descriptor)
                                           ,default_Descriptor_Does_Not_Match_Signed_Hash
     ,Invalid_Signed_Hash(invalid_Signed_Hash_explanation
                         ,invalid_Signed_Hash_signed_hash)
                         ,default_Invalid_Signed_Hash
     ,Descriptor_Does_Not_Match_Crypto_ID(descriptor_Does_Not_Match_Crypto_ID_explanation
                                         ,descriptor_Does_Not_Match_Crypto_ID_crypto_id
                                         ,descriptor_Does_Not_Match_Crypto_ID_crypto_id_type_descriptor)
                                         ,default_Descriptor_Does_Not_Match_Crypto_ID
     ,No_Supported_Crypto_ID_Type_Descriptor_Provided(no_Supported_Crypto_ID_Type_Descriptor_Provided_explanation
                                                     ,no_Supported_Crypto_ID_Type_Descriptor_Provided_supported_crypto_id_type_descriptor
                                                     ,no_Supported_Crypto_ID_Type_Descriptor_Provided_offending_crypto_id_type_descriptor)
                                                     ,default_No_Supported_Crypto_ID_Type_Descriptor_Provided
     ,Descriptor_Does_Not_Match_Public_Crypto_Key(descriptor_Does_Not_Match_Public_Crypto_Key_explanation
                                                 ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key
                                                 ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key_type_descriptor)
                                                 ,default_Descriptor_Does_Not_Match_Public_Crypto_Key
     ,No_Supported_Hash_Type_Descriptor_Provided(no_Supported_Hash_Type_Descriptor_Provided_explanation
                                                ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor
                                                ,no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor)
                                                ,default_No_Supported_Hash_Type_Descriptor_Provided
     ,No_Supported_Hash_Sha2_Descriptor_Provided(no_Supported_Hash_Sha2_Descriptor_Provided_explanation
                                                ,no_Supported_Hash_Sha2_Descriptor_Provided_supported_hash_sha2_descriptor
                                                ,no_Supported_Hash_Sha2_Descriptor_Provided_offending_hash_sha2_descriptor)
                                                ,default_No_Supported_Hash_Sha2_Descriptor_Provided
     ,Proposal_1a
       ,decode_Proposal_1a
     ,Public_Crypto_Key(Public_Crypto_Key
                       ,public_Crypto_Key_public_crypto_key_x509)
     ,Crypto_ID(Crypto_ID, crypto_ID_public_crypto_key)
               ,default_Crypto_ID
     ,Proof_of_Consensus
       ,decode_Proof_of_Consensus
     ,Phase_2b
       ,decode_Phase_2b
     ,Phase_2a
       ,decode_Phase_2a
     ,Phase_1b
       ,decode_Phase_1b
     ,Signed_Hash_Type_Descriptor(Signed_Hash_Type_Descriptor)
        ,signed_Hash_Type_Descriptor_hash_type_descriptor
        ,signed_Hash_Type_Descriptor_crypto_id
        ,default_Signed_Hash_Type_Descriptor
     ,Crypto_ID_Type_Descriptor(Crypto_ID_Type_Descriptor
                               ,crypto_ID_Type_Descriptor_public_crypto_key)
     ,Hash_Type_Descriptor(Hash_Type_Descriptor
                          ,hash_Type_Descriptor_sha2)
                          ,default_Hash_Type_Descriptor
     ,Public_Crypto_Key_Type_Descriptor(Public_Crypto_Key_Type_Descriptor
                                       ,public_Crypto_Key_Type_Descriptor_public_crypto_key_x509)
                                       ,default_Public_Crypto_Key
     ,Hetcons_Message (Hetcons_Message)
        ,hetcons_Message_proposals
        ,hetcons_Message_phase_1as
        ,hetcons_Message_phase_1bs
        ,hetcons_Message_phase_2as
        ,hetcons_Message_index
        ,default_Hetcons_Message
        ,encode_Hetcons_Message
     ,Signed_Index (Signed_Index)
        ,signed_Index_index 
        ,signed_Index_signature 
     ,Phase_1b_Indices (Phase_1b_Indices)
        ,phase_1b_Indices_index_1a
        ,phase_1b_Indices_indices_2a
        ,phase_1b_Indices_signature
     ,Signed_Indices(Signed_Indices)
        ,signed_Indices_indices
        ,signed_Indices_signature
     ,Signed_Hash(Signed_Hash, signed_Hash_crypto_id
                 ,signed_Hash_hash_type_descriptor, signed_Hash_signature)
                 ,default_Signed_Hash
    )

import Crypto.Hash.Algorithms
    ( SHA224(SHA224), SHA256(SHA256), SHA384(SHA384), SHA512(SHA512) )
import Control.Monad (liftM)
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.Logger ( MonadLogger )
import Crypto.Random ( DRG )
import Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Lazy as ByteString (concat)
import Data.Foldable ( Foldable(maximum, null), toList, forM_ )
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Vector ((!))
import qualified Data.Vector as Vector (map, empty)
import GHC.Generics ( Generic )
import Data.Hashable ( Hashable, hashWithSalt )
import Data.HashSet ( HashSet, singleton, intersection )
import qualified Data.HashSet as HashSet ( map )
import Data.Serialize ( Serialize, encodeLazy, decodeLazy )
import Data.Text.Lazy ( pack )
import qualified EasyX509 as X509 ( sign, verify, Signer )
import Thrift.Protocol.Compact ( CompactProtocol(CompactProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-- Janky way to disable debug statemetns instead of import Control.Monad.Logger.CallStack ( logDebugSH )
logDebugSH _ = return () 

-- | For storing data we've verified to be correctly signed
--   Note that the original data (unsigned and parsed) can be easily retreived, as can the signed Message itself.
--   Note that we do not export any constructors for Verified.
--   The only way data should end up in this type is if it's passed through the Verify function.
data Verified a = Verified {
   -- | The original (parsed, but not "verified") datum
   verified_original :: a
   -- | The signed message from which this was parsed and verified
  ,verified_signed :: Maybe (Verified Hetcons_Message)

  }
-- | Verified things are equal precisely when their original signed messages are equal
instance {-# OVERLAPPING #-} Eq (Verified Hetcons_Message) where
  (==) x y = (original x) == (original y)
instance {-# OVERLAPPABLE #-} (Get_Signature_ByteString a) => Eq (Verified a) where
  (==) x y = (signature_bytestring x) == (signature_bytestring y)
-- | We can hash Verified things by hashing their signed message version
instance {-# OVERLAPPING #-} Hashable (Verified Hetcons_Message) where
  hashWithSalt i = hashWithSalt i . original
instance {-# OVERLAPPABLE #-} (Get_Signature_ByteString a) => Hashable (Verified a) where
  hashWithSalt i = hashWithSalt i . signature_bytestring

class Get_Signature a where
  signature :: (Verified a) -> Signed_Hash
instance Get_Signature (Recursive_1a v) where
  signature = signature_1a . original . signed 
signature_1a h1a = signed_Index_signature $ ((hetcons_Message_phase_1as h1a) ! (fromIntegral $ hetcons_Message_index h1a))

instance Get_Signature (Recursive_1b v) where
  signature = signature_1b . original . signed
signature_1b h1b = phase_1b_Indices_signature $ ((hetcons_Message_phase_1bs h1b) ! (fromIntegral $ hetcons_Message_index h1b))

instance Get_Signature (Recursive_2a v) where
  signature = signature_2a . original . signed
signature_2a h2a = signed_Indices_signature $ ((hetcons_Message_phase_2as h2a) ! (fromIntegral $ hetcons_Message_index h2a))

instance Get_Signature (Recursive_2b v) where
  signature = signature_2b . original . signed
signature_2b = signature_2a

class Get_Signature_ByteString a where
  signature_bytestring :: (Verified a) -> ByteString

instance {-# OVERLAPPING #-} Get_Signature_ByteString (Recursive_Proof_of_Consensus v) where
  signature_bytestring = signature_bytestring_proof_of_consensus . original . signed 
signature_bytestring_proof_of_consensus hetcons_message@Hetcons_Message
                                                        {hetcons_Message_phase_2as = phase_2as
                                                        ,hetcons_Message_phase_1bs = phase_1bs
                                                        ,hetcons_Message_index = index
                                                        } = 
    let signed_Indices_to_index_1a = phase_1b_Indices_index_1a . (phase_1bs!) . fromIntegral .  head . toList . signed_Indices_indices
        r2as_with_matching_1a = filter ((== (signed_Indices_to_index_1a $ phase_2as!(fromIntegral index))) . signed_Indices_to_index_1a) $ toList phase_2as
     in ByteString.concat $ sort $ map (signed_Hash_signature . signed_Indices_signature) r2as_with_matching_1a 

instance {-# OVERLAPPABLE #-} (Get_Signature a) => Get_Signature_ByteString a where
  signature_bytestring = signed_Hash_signature . signature


-- | We print out verified stuff by printing the parsed version preceded by "VERIFIED: "
instance (Show a) => Show (Verified a) where
  show = ("VERIFIED:  " ++) . show . original


-- | The original (parsed, but not "verified") datum
--   An accessor function for the `verified_original' field of Verified s.
--   We use this instead of the field name because exporting the field name allows fields to be modified using GHC's foo { bar = baz } syntax.
original :: Verified a -> a
original = verified_original

-- | The signed message from which this darum was parsed and verified.
--   An accessor function for the `verified_signed field of Verified s.
--   We use this instead of the field name because exporting the field name allows fields to be modified using GHC's foo { bar = baz } syntax.
class Signed a where
  signed :: a -> Verified Hetcons_Message
instance {-# OVERLAPPING #-} Signed (Verified Hetcons_Message) where
  signed = id
instance {-# OVERLAPPABLE #-} Signed (Verified a) where
  signed = fromJust . verified_signed


-- | TODO: With the advent of Hetcons_Message reformatting, this is no longer a sane thing to do. I'm removing this so we can fix stuff.
--   We want to parse incoming messages into their `recursive' version, which is
--    to say we will verify that message, and also any messages its carrying in
--    any of their fields, and store the original signed messages as well as the
--    parsed data structures.
--   This class defines what the Recursive version of a basic Thrift datatype is.
-- class Recursive a b where
  -- | Sometimes it's useful to have access to the basic thrift data structure.
  --   This function should grant that access.
--   non_recursive :: b -> a



-- | Proposal_1a s carry no signed messages within them, but their recursive version can fill in some stuff, like calculating quorums from an observer graph
--   Therefore we store both the original, non_recursive version, and the "filled-in" version.
data Recursive_1a v = Recursive_1a {
   -- | The original non-recursive version from which this is parsed
   recursive_1a_non_recursive ::Proposal_1a
   -- | The "filled-in" version, in which we have, for example, calculated Quorums from the observer graph.
  ,recursive_1a_filled_in :: Proposal_1a
   -- | The parsed value
  ,recursive_1a_value :: v
  } deriving (Show, Eq, Generic)
instance (Serialize v) => Serialize (Recursive_1a v) -- I'm not clear on why this is necessary, but the compiler asks for it to derive Eq for Recursive_1b




-- | Phase_1b s carry 1a and 2a messages with them.
--   Recursive_1b s carry parsed and verified versions of these.
data Recursive_1b v = Recursive_1b {
   -- | Hetcons_Message formatting removes the need for The original, non-recursive version from which this is parsed
   -- recursive_1b_non_recursive :: Phase_1b
   -- | The Recursive version of the 1B's contained 1A
   recursive_1b_proposal :: Verified (Recursive_1a v)
   -- | The Recursive version of the 1B's contained 2As
  ,recursive_1b_conflicting_phase2as :: (HashSet (Verified (Recursive_2a v)))
} deriving (Show, Eq, Generic)



-- | Phase_2a s carry phase 1b messages with them.
--   Recursive_2a s carry parsed and verified versions of these.
newtype Recursive_2a v = Recursive_2a (HashSet (Verified (Recursive_1b v))) deriving (Show, Eq, Generic)



-- | Phase_2b s carry signed 1b messages with them.
--   Recursive_2bs carry parsed and verified versions of these.
newtype Recursive_2b v = Recursive_2b (HashSet (Verified (Recursive_1b v))) deriving (Show, Eq, Generic)

-- | Proof_of_Consensus messages carry signed 2b messages with them.
--   Recursive_Proof_of_Consensus objects carry parsed and verified versions of these.
newtype Recursive_Proof_of_Consensus v = Recursive_Proof_of_Consensus (HashSet (Verified (Recursive_2b v))) deriving (Show, Eq, Generic)


class From_Hetcons_Message a where
  from_Hetcons_Message :: (Verified Hetcons_Message) -> a

instance (Monad m) => From_Hetcons_Message (m Hetcons_Message) where
  from_Hetcons_Message = return . original

class (Monad m) => To_Hetcons_Message m a where
  to_Hetcons_Message :: a -> (m Hetcons_Message)

instance (Monad m, MonadLogger m) => To_Hetcons_Message m Hetcons_Message where
  to_Hetcons_Message x = do { logDebugSH "to_Hetcons_Message Hetcons_Message"
                            ; return x}

instance (Monad m, MonadLogger m) => To_Hetcons_Message m (Verified Hetcons_Message) where
  to_Hetcons_Message x = do {logDebugSH "to_Hetcons_Message (Verified Hetcons_Message)"
                            ;return $ original x}

instance (Monad m, MonadLogger m) => To_Hetcons_Message m (Verified a) where
  to_Hetcons_Message x = do {logDebugSH "to_Hetcons_Message Verified"
                            ;return $ original $ signed x}

-- | verify is only way to construct a Verified object.
--   If all goes well, you get a verified version of the Parsable type (e.g. Recursive_1b) specified.
--   Otherwise, you get an exception.
--   Verify is Memoized, and thus should be called in a Monad which maintains its memoization cache.
--   Such a monad is Hetcons_Transaction, defined in Receive_Message.
--   verify should be a memoized version of verify'
class (MonadError Hetcons_Exception m, From_Hetcons_Message (m a)) => Monad_Verify a m where
  -- | verify is only way to construct a Verified object.
  --   If all goes well, you get a verified version of the Verifiable type (e.g. Recursive_1b) specified.
  --   Otherwise, you get an exception.
  verify :: Hetcons_Message -> m (Verified a)







-- | returns the correct (longest allowed) sha2 length to use given a set of possible lengths, or an exception
sha2_length :: (MonadError Hetcons_Exception m, Integral a, Num b) => HashSet a -> m b
sha2_length length_set =
  let hash_sha2_descriptor = HashSet.map fromIntegral length_set
      lengths = intersection sUPPORTED_HASH_SHA2_DESCRIPTOR hash_sha2_descriptor
   in if null lengths
         then throwError $ Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided default_No_Supported_Hash_Sha2_Descriptor_Provided {
                 no_Supported_Hash_Sha2_Descriptor_Provided_offending_hash_sha2_descriptor = Just hash_sha2_descriptor
                ,no_Supported_Hash_Sha2_Descriptor_Provided_supported_hash_sha2_descriptor = Just sUPPORTED_HASH_SHA2_DESCRIPTOR
                ,no_Supported_Hash_Sha2_Descriptor_Provided_explanation = Just  "I do not support that length of SHA2 hash"}
         else return $ fromIntegral $ maximum lengths


-- | This is the only pure way to construct a Verified Hetcons_Message.
--   Otherwise, you get an exception.
verify_hetcons_message :: (Encodable Proposal_1a, Monad_Verify_Quorums m,  MonadError Hetcons_Exception m) => Hetcons_Message -> m (Verified Hetcons_Message)
-- We first verify that the Hetcons_Message itself is good (all signatures correct and such)
verify_hetcons_message message@(Hetcons_Message
                  {hetcons_Message_proposals = proposals
                  ,hetcons_Message_phase_1as = phase_1as
                  ,hetcons_Message_phase_1bs = phase_1bs
                  ,hetcons_Message_phase_2as = phase_2as
                  }) = do
  {let binary_proposals = Vector.map encode proposals
  ;forM_ phase_1as (\(Signed_Index{signed_Index_index = index
                                  ,signed_Index_signature = signed_hash}) -> (
                      verify_bytestring (binary_proposals!(fromIntegral index)) signed_hash))
  ;forM_ phase_1bs (\(Phase_1b_Indices  { phase_1b_Indices_index_1a = index_1a
                                        , phase_1b_Indices_indices_2a = indices_2a
                                        , phase_1b_Indices_signature = signed_hash}) -> (
                      verify_bytestring (ByteString.concat
                                          ((signed_Hash_signature $ signed_Index_signature $ phase_1as!(fromIntegral index_1a)):
                                           (sort $ map (signed_Hash_signature . signed_Indices_signature . (phase_2as!) . fromIntegral)
                                                       $ toList indices_2a)))
                                        signed_hash))
  ;forM_ phase_2as (\(Signed_Indices{signed_Indices_indices = indices
                                    ,signed_Indices_signature = signed_hash}) -> (
                      verify_bytestring (ByteString.concat $ sort $ map (signed_Hash_signature . phase_1b_Indices_signature . (phase_1bs!) . fromIntegral)
                                                                        $ toList indices)
                                        signed_hash))
  -- now that we've checked all the signatures, we can construct whatever data type is desired:
  ;return Verified{verified_original = message, verified_signed = Nothing}
  }

alter_hetcons_index :: (Integral a) => a -> (Verified Hetcons_Message) -> (Verified Hetcons_Message)
alter_hetcons_index i v = Verified{verified_original = (original v){hetcons_Message_index = fromIntegral i}, verified_signed = Nothing}

-- | This is the only pure way to construct a Verified object.
--   If all goes well, you get a verified version of the Parsable type (e.g. Recursive_1b) specified.
--   Otherwise, you get an exception.
verify' :: (Monad_Verify Hetcons_Message m, From_Hetcons_Message (m a)) => Hetcons_Message -> m (Verified a)
verify' hetcons_message = do
  {verified_hetcons_message <- verify hetcons_message
  ;value <- from_Hetcons_Message verified_hetcons_message
  ;return Verified{verified_original = value, verified_signed = Just verified_hetcons_message}
  }

-- verify that a bytestring is signed correctly.
verify_bytestring :: ( MonadError Hetcons_Exception m) => ByteString -> Signed_Hash -> m ()
-- in the case where all is done correctly:
verify_bytestring
        payload
        signed_hash@Signed_Hash
          {signed_Hash_signature = signature
          ,signed_Hash_hash_type_descriptor = Just
             Hash_Type_Descriptor
             {hash_Type_Descriptor_sha2 = Just hash_sha2_descriptor}
          ,signed_Hash_crypto_id = Just
             Crypto_ID
             {crypto_ID_public_crypto_key = Just
                Public_Crypto_Key
                  {public_Crypto_Key_public_crypto_key_x509 = Just public_key}}}
  =
    do{max_length <- sha2_length hash_sha2_descriptor
      ;let verify_with_length = case max_length of
                                     28 -> X509.verify $ Just SHA224
                                     32 -> X509.verify $ Just SHA256
                                     48 -> X509.verify $ Just SHA384
                                     _  -> X509.verify $ Just SHA512
      ;case verify_with_length public_key payload signature of
         (Just e) ->
           throwError $ Hetcons_Exception_Invalid_Signed_Hash default_Invalid_Signed_Hash {
              invalid_Signed_Hash_signed_hash = signed_hash
             ,invalid_Signed_Hash_explanation = Just $ pack e}
         Nothing -> return ()}

-- | If it's a public crypto key, but not an x509, we return an appropriate Exception
verify_bytestring _
        (Signed_Hash
          {signed_Hash_crypto_id = Just
             Crypto_ID
             {crypto_ID_public_crypto_key = Just
                public_key@(Public_Crypto_Key
                  {public_Crypto_Key_public_crypto_key_x509 = Nothing})}}) -- it's a public crypto key, but not an x509
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
             default_Descriptor_Does_Not_Match_Public_Crypto_Key {
                descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key_type_descriptor =
                  sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key =
                  public_key
               ,descriptor_Does_Not_Match_Public_Crypto_Key_explanation =
                  Just "Alas, I only support X509 Public Crypto Keys at this time."}

-- | If it's a Crypto ID, but not a public crypto key, we return an appropriate Exception
verify_bytestring _
        (Signed_Hash
          {signed_Hash_crypto_id = Just
             crypto_id@(Crypto_ID
             {crypto_ID_public_crypto_key = Nothing})})
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
             default_Descriptor_Does_Not_Match_Crypto_ID {
                descriptor_Does_Not_Match_Crypto_ID_crypto_id_type_descriptor =
                  sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Crypto_ID_crypto_id =
                  crypto_id
               ,descriptor_Does_Not_Match_Crypto_ID_explanation =
                  Just "Alas, I require crypto IDs to be full X509 Public Crypto Keys at this time."}

-- | If the signed Hash doesn't include a Crypto_ID, we return an appropriate exception
verify_bytestring _ signed_hash@Signed_Hash {signed_Hash_crypto_id = Nothing}
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
             default_Descriptor_Does_Not_Match_Signed_Hash {
                descriptor_Does_Not_Match_Signed_Hash_signed_hash_type_descriptor =
                  sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Signed_Hash_signed_hash =
                  signed_hash
               ,descriptor_Does_Not_Match_Signed_Hash_explanation =
                  Just "I require this signed hash to carry a crypto ID"}

-- | If the signed Hash specifies a type, but it isn't sha2, we return an appropriate exception
verify_bytestring _
        (Signed_Hash
          {signed_Hash_hash_type_descriptor = hash_type_descriptor@(Just
             Hash_Type_Descriptor
             {hash_Type_Descriptor_sha2 = Nothing})})
  = throwError $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
             default_No_Supported_Hash_Type_Descriptor_Provided {
                no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor =
                  hash_type_descriptor
               ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor =
                  Just sUPPORTED_HASH_TYPE_DESCRIPTOR
               ,no_Supported_Hash_Type_Descriptor_Provided_explanation =
                  Just "Alas, I only support SHA2 hashes at this time."}

-- | If the signed Hash does not specify a type, we return an appropriate exception
verify_bytestring _ signed_hash@Signed_Hash {signed_Hash_hash_type_descriptor = Nothing}
  = throwError $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
             default_No_Supported_Hash_Type_Descriptor_Provided  {
               no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor = Nothing
              ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor = Just sUPPORTED_HASH_TYPE_DESCRIPTOR
              ,no_Supported_Hash_Type_Descriptor_Provided_explanation = Just "No Hash Type Descriptor provided"}



-- | a class for those types which Thrift can encode. Alas, thrift doesn't seem to provide this itself.
class Encodable a where
  -- | encode it to a Lazy ByteString using Thrift's encode function with CompactProtocol, EmptyTransport.
  --   Instantiations for all the types we care about are in their Instances_XX files.
  --   For testing purposes, Test.Util provides instance Serialize a => Encodable a
  encode :: a -> ByteString

instance {-# OVERLAPPING #-} Encodable Hetcons_Message where
  encode = encode_Hetcons_Message (CompactProtocol EmptyTransport)

-- | builds a Signed_Message given a signing key, a matching certificate, something serializable, etc.
sign ::(MonadError Hetcons_Exception m, X509.Signer signer, DRG gen) =>
       Crypto_ID -> signer -> Signed_Hash_Type_Descriptor -> gen -> ByteString -> m Signed_Hash
-- | If everything is correct, and we've got just an x509 public key
sign (Crypto_ID
       {crypto_ID_public_crypto_key = Just
          Public_Crypto_Key
          {public_Crypto_Key_public_crypto_key_x509 = Just public_key}})
     private_key
     (Signed_Hash_Type_Descriptor
       {signed_Hash_Type_Descriptor_hash_type_descriptor = Just
          Hash_Type_Descriptor
          {hash_Type_Descriptor_sha2 = Just hash_sha2_descriptor}
       ,signed_Hash_Type_Descriptor_crypto_id = Just
          Crypto_ID_Type_Descriptor
          {crypto_ID_Type_Descriptor_public_crypto_key = Just
             Public_Crypto_Key_Type_Descriptor
               {public_Crypto_Key_Type_Descriptor_public_crypto_key_x509 = Just True}}})
     random_generator
     payload
  =
    do{max_length <- sha2_length hash_sha2_descriptor
      ;let sign_with_length = case max_length of
                                   28 -> X509.sign $ Just SHA224
                                   32 -> X509.sign $ Just SHA256
                                   48 -> X509.sign $ Just SHA384
                                   _  -> X509.sign $ Just SHA512
      ;signature <- case sign_with_length random_generator private_key payload of
                      Left e -> throwError (Hetcons_Exception_Invalid_Signed_Hash default_Invalid_Signed_Hash {
                               invalid_Signed_Hash_explanation = Just $ pack (
                                 "Something went wrong while trying to sign with this key:\n" ++ e)})
                      Right s -> return s
      ;return default_Signed_Hash {
                 signed_Hash_signature = signature
                ,signed_Hash_crypto_id = Just default_Crypto_ID
                   {crypto_ID_public_crypto_key = Just default_Public_Crypto_Key
                      {public_Crypto_Key_public_crypto_key_x509 = Just public_key}}
                ,signed_Hash_hash_type_descriptor = Just default_Hash_Type_Descriptor
                     {hash_Type_Descriptor_sha2 = Just $ singleton max_length}}}


-- | If the Crypto_ID is a key, but not an X509
sign (Crypto_ID
       {crypto_ID_public_crypto_key = Just
          public_key@Public_Crypto_Key
          {public_Crypto_Key_public_crypto_key_x509 = Nothing}})
      _ _ _ _
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
             default_Descriptor_Does_Not_Match_Public_Crypto_Key {
                descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key_type_descriptor =
                  sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key =
                  public_key
               ,descriptor_Does_Not_Match_Public_Crypto_Key_explanation =
                  Just "Alas, I only support X509 Public Crypto Keys at this time."}

-- | If the crypto ID isn't a public key
sign crypto_id@(Crypto_ID {crypto_ID_public_crypto_key = Nothing}) _ _ _ _ =
  throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
           default_Descriptor_Does_Not_Match_Crypto_ID {
              descriptor_Does_Not_Match_Crypto_ID_crypto_id_type_descriptor =
                sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
             ,descriptor_Does_Not_Match_Crypto_ID_crypto_id =
                crypto_id
             ,descriptor_Does_Not_Match_Crypto_ID_explanation =
                Just "Alas, I require crypto IDs to be full X509 Public Crypto Keys at this time."}

-- | If the Signed_Hash_Type_Descriptor calls for a public key, but not an X509
sign (Crypto_ID {crypto_ID_public_crypto_key = Just key})
     _
     (Signed_Hash_Type_Descriptor
       {signed_Hash_Type_Descriptor_hash_type_descriptor = Just
          Hash_Type_Descriptor
          {hash_Type_Descriptor_sha2 = Just hash_sha2_descriptor}
       ,signed_Hash_Type_Descriptor_crypto_id = Just
          Crypto_ID_Type_Descriptor
          {crypto_ID_Type_Descriptor_public_crypto_key = Just
             descriptor@Public_Crypto_Key_Type_Descriptor
               {public_Crypto_Key_Type_Descriptor_public_crypto_key_x509 = _}}})
     _ _
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
             default_Descriptor_Does_Not_Match_Public_Crypto_Key {
                descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key_type_descriptor =
                  descriptor
               ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key =
                  key
               ,descriptor_Does_Not_Match_Public_Crypto_Key_explanation =
                  Just "You provided an X509, but asked for something else"}

-- | If the Signed_Hash_Type_Descriptor calls for a crypto_ID which is not a public key
sign crypto_id@(Crypto_ID {crypto_ID_public_crypto_key = Just _})
     _
     (Signed_Hash_Type_Descriptor
       {signed_Hash_Type_Descriptor_hash_type_descriptor = Just
          Hash_Type_Descriptor
          {hash_Type_Descriptor_sha2 = Just hash_sha2_descriptor}
       ,signed_Hash_Type_Descriptor_crypto_id = Just
          descriptor@Crypto_ID_Type_Descriptor
          {crypto_ID_Type_Descriptor_public_crypto_key = Nothing}})
     _ _
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
             default_Descriptor_Does_Not_Match_Crypto_ID {
                descriptor_Does_Not_Match_Crypto_ID_crypto_id_type_descriptor =
                  descriptor
               ,descriptor_Does_Not_Match_Crypto_ID_crypto_id =
                  crypto_id
               ,descriptor_Does_Not_Match_Crypto_ID_explanation =
                  Just "You provided a public key, but asked for something else"}


-- | If the Signed_Hash_Type_Descriptor does not call for a crypto_ID
sign crypto_id@(Crypto_ID {crypto_ID_public_crypto_key = Just _})
     _
     (Signed_Hash_Type_Descriptor
       {signed_Hash_Type_Descriptor_hash_type_descriptor = Just
          Hash_Type_Descriptor
          {hash_Type_Descriptor_sha2 = Just hash_sha2_descriptor}
       ,signed_Hash_Type_Descriptor_crypto_id = Nothing})
     _ _
  = throwError $ Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided
             default_No_Supported_Crypto_ID_Type_Descriptor_Provided  {
                no_Supported_Crypto_ID_Type_Descriptor_Provided_offending_crypto_id_type_descriptor = Nothing
               ,no_Supported_Crypto_ID_Type_Descriptor_Provided_supported_crypto_id_type_descriptor = Just sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
               ,no_Supported_Crypto_ID_Type_Descriptor_Provided_explanation = Just "You did not provide any Crypto_ID_Type_Descriptor at all"}

-- | If the Signed_Hash_Type_Descriptor specifies a Hash, but it's not SHA2
sign _ _
     (Signed_Hash_Type_Descriptor
       {signed_Hash_Type_Descriptor_hash_type_descriptor = Just
          hash_type_descriptor@Hash_Type_Descriptor
          {hash_Type_Descriptor_sha2 = Nothing}})
     _ _
  = throwError $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
             default_No_Supported_Hash_Type_Descriptor_Provided {
                no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor =
                  Just hash_type_descriptor
               ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor =
                  Just sUPPORTED_HASH_TYPE_DESCRIPTOR
               ,no_Supported_Hash_Type_Descriptor_Provided_explanation =
                  Just "Alas, I only support SHA2 hashes at this time."}

-- | If the Signed_Hash_Type_Descriptor specifies a Hash, but it's not SHA2
sign _ _ (Signed_Hash_Type_Descriptor {signed_Hash_Type_Descriptor_hash_type_descriptor = Nothing}) _ _ =
  throwError $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
           default_No_Supported_Hash_Type_Descriptor_Provided  {
             no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor = Nothing
            ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor = Just sUPPORTED_HASH_TYPE_DESCRIPTOR
            ,no_Supported_Hash_Type_Descriptor_Provided_explanation = Just "No Hash Type Descriptor provided"}
