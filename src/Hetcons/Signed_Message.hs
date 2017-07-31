{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Signed_Message
    (sign
    ,Monad_Verify
       ,verify
       ,verify'
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
import Hetcons.Quorums ( Monad_Verify_Quorums )
import Hetcons.Serializable ()

import Hetcons_Consts
    ( sUPPORTED_HASH_SHA2_DESCRIPTOR
     ,sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
     ,sUPPORTED_HASH_TYPE_DESCRIPTOR
     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
     ,sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR )
import Hetcons_Types
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
     ,Signed_Message (Signed_Message)
                        ,signed_Message_payload
                        ,signed_Message_signature
                        ,default_Signed_Message
     ,Signed_Hash(Signed_Hash, signed_Hash_crypto_id
                 ,signed_Hash_hash_type_descriptor, signed_Hash_signature)
                 ,default_Signed_Hash
    )

import Crypto.Hash.Algorithms
    ( SHA224(SHA224), SHA256(SHA256), SHA384(SHA384), SHA512(SHA512) )
import Control.Monad.Except ( MonadError(throwError) )
import Crypto.Random ( DRG )
import Data.ByteString.Lazy ( ByteString )
import Data.Foldable ( Foldable(maximum, null) )
import GHC.Generics ( Generic )
import Data.Hashable ( Hashable, hashWithSalt )
import Data.HashSet ( HashSet, singleton, intersection )
import qualified Data.HashSet as HashSet ( map )
import Data.Serialize ( Serialize, encodeLazy, decodeLazy )
import Data.Text.Lazy ( pack )
import qualified EasyX509 as X509 ( sign, verify, Signer )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

 -- | For storing data we've verified to be correctly signed
 -- | Note that the original data (unsigned and parsed) can be easily retreived, as can the signed Message itself.
 -- | Note that we do not export any constructors for Verified.
 -- | The only way data should end up in this type is if it's passed through the Verify function.
data (Parsable a) => Verified a = Verified {
   verified_original :: a
  ,verified_signed   :: Signed_Message
  }
instance (Parsable a) => Eq (Verified a) where
  (==) x y = (signed x) == (signed y)
instance (Parsable a) => Hashable (Verified a) where
  hashWithSalt i = hashWithSalt i . signed
instance (Parsable a, Show a) => Show (Verified a) where
  show = ("VERIFIED:  " ++) . show . original


-- | An accessor function for the `verified_original' field of Verified s.
-- | We use this instead of the field name because exporting the field name allows fields to be modified using GHC's foo { bar = baz } syntax.
original :: (Parsable a) => Verified a -> a
original = verified_original

-- | An accessor function for the `verified_signed field of Verified s.
-- | We use this instead of the field name because exporting the field name allows fields to be modified using GHC's foo { bar = baz } syntax.
signed :: (Parsable a) => Verified a -> Signed_Message
signed = verified_signed


-- | We want to parse incoming messages into their `recursive' version, which is to say we will verify that message, and also any messages its carrying in any of their fields, and store the original signed messages as well as the parsed data structures.
-- | However, sometimes it's useful to have access to the basic thrift data structure.
-- | This function should grand that access.
class Recursive a b where
  non_recursive :: b -> a



-- | Proposal_1a s carry no signed messages within them, but their recursive version can fill in some stuff, like quorums
data Recursive_1a = Recursive_1a {
   recursive_1a_non_recursive ::Proposal_1a
  ,recursive_1a_filled_in :: Proposal_1a
  } deriving (Show, Eq, Generic)
instance Serialize Recursive_1a -- I'm not clear on why this is necessary, but the compiler asks for it to derive Eq for Recursive_1b




-- | Phase_1b s carry 1a and 2a messages with them.
-- | Recursive_1b s carry parsed and verified versions of these.
data Recursive_1b = Recursive_1b {
   recursive_1b_non_recursive :: Phase_1b
  ,recursive_1b_proposal :: Verified Recursive_1a
  ,recursive_1b_conflicting_phase2as :: (HashSet (Verified Recursive_2a))
  } deriving (Generic)
instance Show Recursive_1b
instance Eq Recursive_1b



-- | Phase_2a s carry phase 1b messages with them.
-- | Recursive_2a s carry parsed and verified versions of these.
newtype Recursive_2a = Recursive_2a (HashSet (Verified Recursive_1b)) deriving (Generic)
instance Show Recursive_2a
instance Eq Recursive_2a



-- | Phase_2b s carry signed 1b messages with them.
-- | Recursive_2bs carry parsed and verified versions of these.
newtype Recursive_2b = Recursive_2b (HashSet (Verified Recursive_1b)) deriving (Generic)
instance Show Recursive_2b
instance Eq Recursive_2b

-- | Proof_of_Consensus messages carry signed 2b messages with them.
-- | Recursive_Proof_of_Consensus objects carry parsed and verified versions of these.
newtype Recursive_Proof_of_Consensus = Recursive_Proof_of_Consensus (HashSet (Verified Recursive_2b)) deriving (Generic)
instance Show Recursive_Proof_of_Consensus
instance Eq Recursive_Proof_of_Consensus


-- | We have messages serialized for transport within Signed_Messages.
-- | However, deserializing them into their thrift data structures is not enough, if we want to recursively parse and verify the signed messags they carry within themselves.
-- | Therefore, we create the Parsable class for stuff which might require such recursive verification.
class Parsable a where
  -- | The parse function is meant to deserialize an object, but also deserialize and verify any signed messages within it.
  -- | Of course, this depends on the type of the object.
  parse :: (Monad_Verify Recursive_1a m
           ,Monad_Verify Recursive_1b m
           ,Monad_Verify Recursive_2a m
           ,Monad_Verify Recursive_2b m
           ,Monad_Verify Recursive_Proof_of_Consensus m
           ,Monad_Verify_Quorums m
           ,MonadError Hetcons_Exception m) => ByteString -> m a

-- | By default, anythign serializable is simply deserialized.
-- | the only possible error is if parsing fails
instance {-# OVERLAPPING #-} Parsable Proposal_1a where
  parse = return . (decode_Proposal_1a (BinaryProtocol EmptyTransport))
instance {-# OVERLAPPING #-} Parsable Phase_1b where
  parse = return . (decode_Phase_1b (BinaryProtocol EmptyTransport))
instance {-# OVERLAPPING #-} Parsable Phase_2a where
  parse = return . (decode_Phase_2a (BinaryProtocol EmptyTransport))
instance {-# OVERLAPPING #-} Parsable Phase_2b where
  parse = return . (decode_Phase_2b (BinaryProtocol EmptyTransport))
instance {-# OVERLAPPING #-} Parsable Proof_of_Consensus where
  parse = return . (decode_Proof_of_Consensus (BinaryProtocol EmptyTransport))

instance {-# OVERLAPPABLE #-} Serialize a => Parsable a where
  parse payload =
    case decodeLazy payload of
      (Left e) ->
        throwError $ Hetcons_Exception_Unparsable_Hashable_Message default_Unparsable_Hashable_Message {
           unparsable_Hashable_Message_message = payload
          ,unparsable_Hashable_Message_explanation = Just $ pack $ "I was unable to parse this message payload:\n" ++ e}
      (Right x) -> return x






-- | This is the only way to construct a Verified object.
-- | If all goes well, you get a verified version of the Parsable type (e.g. Recursive_1b) specified.
-- | Otherwise, you get an exception.
-- | TODO? We may want to make verify memoized. In theory, all that is necessary is to make verify = memoize verify'
-- |       For some reason, as of 2017-6-26, this actually slows down our unit tests.
-- |       Basic memoize tests on, say, fibonacci seem to work fine.

class (MonadError Hetcons_Exception m, Parsable a) => Monad_Verify a m where
  verify :: Signed_Message -> m (Verified a)

-- TODO: this instance is used in testing only, so we should move it over to tests
instance {-# OVERLAPPABLE #-} (Parsable Recursive_1a
                              ,Parsable Recursive_1b
                              ,Parsable Recursive_2a
                              ,Parsable Recursive_2b
                              ,Parsable Recursive_Proof_of_Consensus
                              ,Monad_Verify_Quorums m
                              ,MonadError Hetcons_Exception m, Parsable a)
                               => Monad_Verify a m where
  verify = verify'






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


-- | This is the only way to construct a Verified object.
-- | If all goes well, you get a verified version of the Parsable type (e.g. Recursive_1b) specified.
-- | Otherwise, you get an exception.
verify' :: (Monad_Verify Recursive_1a m
           ,Monad_Verify Recursive_1b m
           ,Monad_Verify Recursive_2a m
           ,Monad_Verify Recursive_2b m
           ,Monad_Verify Recursive_Proof_of_Consensus m
           ,Monad_Verify_Quorums m
           ,Monad_Verify a m, MonadError Hetcons_Exception m, Parsable a) => Signed_Message -> m (Verified a)
-- In the case where everything's done correctly:
verify' signed_message@Signed_Message
       {signed_Message_payload = payload
       ,signed_Message_signature =
          signed_hash@Signed_Hash
          {signed_Hash_signature = signature
          ,signed_Hash_hash_type_descriptor = Just
             Hash_Type_Descriptor
             {hash_Type_Descriptor_sha2 = Just hash_sha2_descriptor}
          ,signed_Hash_crypto_id = Just
             Crypto_ID
             {crypto_ID_public_crypto_key = Just
                Public_Crypto_Key
                  {public_Crypto_Key_public_crypto_key_x509 = Just public_key}}}}
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
         Nothing ->
           do { parsed_payload <- parse payload
              ; return Verified { verified_original = parsed_payload, verified_signed = signed_message }}}

-- | If it's a public crypto key, but not an x509, we return an appropriate Exception
verify' Signed_Message
       {signed_Message_signature =
          Signed_Hash
          {signed_Hash_crypto_id = Just
             Crypto_ID
             {crypto_ID_public_crypto_key = Just
                public_key@(Public_Crypto_Key
                  {public_Crypto_Key_public_crypto_key_x509 = Nothing})}}} -- it's a public crypto key, but not an x509
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
             default_Descriptor_Does_Not_Match_Public_Crypto_Key {
                descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key_type_descriptor =
                  sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key =
                  public_key
               ,descriptor_Does_Not_Match_Public_Crypto_Key_explanation =
                  Just "Alas, I only support X509 Public Crypto Keys at this time."}

-- | If it's a Crypto ID, but not a public crypto key, we return an appropriate Exception
verify' Signed_Message
       {signed_Message_signature =
          Signed_Hash
          {signed_Hash_crypto_id = Just
             crypto_id@(Crypto_ID
             {crypto_ID_public_crypto_key = Nothing})}}
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
             default_Descriptor_Does_Not_Match_Crypto_ID {
                descriptor_Does_Not_Match_Crypto_ID_crypto_id_type_descriptor =
                  sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Crypto_ID_crypto_id =
                  crypto_id
               ,descriptor_Does_Not_Match_Crypto_ID_explanation =
                  Just "Alas, I require crypto IDs to be full X509 Public Crypto Keys at this time."}

-- | If the signed Hash doesn't include a Crypto_ID, we return an appropriate exception
verify' Signed_Message
       {signed_Message_signature =
          signed_hash@Signed_Hash
          {signed_Hash_crypto_id = Nothing}}
  = throwError $ Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
             default_Descriptor_Does_Not_Match_Signed_Hash {
                descriptor_Does_Not_Match_Signed_Hash_signed_hash_type_descriptor =
                  sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Signed_Hash_signed_hash =
                  signed_hash
               ,descriptor_Does_Not_Match_Signed_Hash_explanation =
                  Just "I require this signed hash to carry a crypto ID"}

-- | If the signed Hash specifies a type, but it isn't sha2, we return an appropriate exception
verify' Signed_Message
       {signed_Message_signature =
          Signed_Hash
          {signed_Hash_hash_type_descriptor = hash_type_descriptor@(Just
             Hash_Type_Descriptor
             {hash_Type_Descriptor_sha2 = Nothing})}}
  = throwError $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
             default_No_Supported_Hash_Type_Descriptor_Provided {
                no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor =
                  hash_type_descriptor
               ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor =
                  Just sUPPORTED_HASH_TYPE_DESCRIPTOR
               ,no_Supported_Hash_Type_Descriptor_Provided_explanation =
                  Just "Alas, I only support SHA2 hashes at this time."}

-- | If the signed Hash does not specify a type, we return an appropriate exception
verify' Signed_Message
       {signed_Message_signature =
          signed_hash@Signed_Hash
          {signed_Hash_hash_type_descriptor = Nothing}}
  = throwError $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
             default_No_Supported_Hash_Type_Descriptor_Provided  {
               no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor = Nothing
              ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor = Just sUPPORTED_HASH_TYPE_DESCRIPTOR
              ,no_Supported_Hash_Type_Descriptor_Provided_explanation = Just "No Hash Type Descriptor provided"}



-- | builds a Signed_Message given a signing key, a matching certificate, something serializable, etc.
sign ::(MonadError Hetcons_Exception m, Serialize serialize, X509.Signer signer, DRG gen) =>
       Crypto_ID -> signer -> Signed_Hash_Type_Descriptor -> gen -> serialize -> m Signed_Message
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
      ;let serialized_payload = encodeLazy payload
      ;signature <- case sign_with_length random_generator private_key serialized_payload of
                      Left e -> throwError (Hetcons_Exception_Invalid_Signed_Hash default_Invalid_Signed_Hash {
                               invalid_Signed_Hash_explanation = Just $ pack (
                                 "Something went wrong while trying to sign with this key:\n" ++ e)})
                      Right s -> return s
      ;return default_Signed_Message { -- and now we describe the entire signed message structure
            signed_Message_payload = serialized_payload
           ,signed_Message_signature = default_Signed_Hash {
               signed_Hash_signature = signature
              ,signed_Hash_crypto_id = Just default_Crypto_ID
                 {crypto_ID_public_crypto_key = Just default_Public_Crypto_Key
                    {public_Crypto_Key_public_crypto_key_x509 = Just public_key}}
              ,signed_Hash_hash_type_descriptor = Just default_Hash_Type_Descriptor
                   {hash_Type_Descriptor_sha2 = Just $ singleton max_length}}}}


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
