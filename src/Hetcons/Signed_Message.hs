{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Signed_Message
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
    ) where

import Hetcons.Hetcons_Exception (
     Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                      ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                      ,Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                      ,Hetcons_Exception_Invalid_Signed_Hash
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                      ,Hetcons_Exception_Unparsable_Hashable_Message)
    )
import Hetcons.Memoize (memoize)
import Hetcons.Quorums (verify_quorums)
import Hetcons.Serializable()

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
                                        ,length
                                        ,maximum)
import           GHC.Generics           (Generic)
import           Data.Hashable          (Hashable
                                        ,hashWithSalt)
import           Data.HashSet           (HashSet
                                        ,intersection
                                        ,toList
                                        ,fromList
                                        ,singleton)
import qualified Data.HashSet as HashSet(map)
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
  parse :: ByteString -> Either Hetcons_Exception a

-- | By default, anythign serializable is simply deserialized.
-- | the only possible error is if parsing fails
instance {-# OVERLAPPABLE #-} Serialize a => Parsable a where
  parse payload =
    case decodeLazy payload of
      (Left e) ->
        Left $ Hetcons_Exception_Unparsable_Hashable_Message default_Unparsable_Hashable_Message {
           unparsable_Hashable_Message_message = payload
          ,unparsable_Hashable_Message_explanation = Just $ pack $ "I was unable to parse this message payload:\n" ++ e}
      (Right x) -> Right x





-- TODO: change these things that return Either to be MonadError Hetcons_Exception m => m a

-- | This is the only way to construct a Verified object.
-- | If all goes well, you get a verified version of the Parsable type (e.g. Recursive_1b) specified.
-- | Otherwise, you get an exception.
-- | TODO? We may want to make verify memoized. In theory, all that is necessary is to make verify = memoize verify'
-- |       For some reason, as of 2017-6-12, this actually slows down our unit tests.
-- |       Basic memoize tests on, say, fibonacci seem to work fine.
verify :: (Parsable a) => Signed_Message -> Either Hetcons_Exception (Verified a)
verify = verify'







-- | returns the correct (longest allowed) sha2 length to use given a set of possible lengths, or an exception
sha2_length :: (Integral a, Num b) => HashSet a -> Either Hetcons_Exception b
sha2_length length_set =
  let hash_sha2_descriptor = HashSet.map fromIntegral length_set
      lengths = intersection sUPPORTED_HASH_SHA2_DESCRIPTOR hash_sha2_descriptor
   in if null lengths
         then Left $ Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided default_No_Supported_Hash_Sha2_Descriptor_Provided {
                 no_Supported_Hash_Sha2_Descriptor_Provided_offending_hash_sha2_descriptor = Just hash_sha2_descriptor
                ,no_Supported_Hash_Sha2_Descriptor_Provided_supported_hash_sha2_descriptor = Just sUPPORTED_HASH_SHA2_DESCRIPTOR
                ,no_Supported_Hash_Sha2_Descriptor_Provided_explanation = Just  "I do not support that length of SHA2 hash"}
         else Right $ fromIntegral $ maximum lengths


-- | This is the only way to construct a Verified object.
-- | If all goes well, you get a verified version of the Parsable type (e.g. Recursive_1b) specified.
-- | Otherwise, you get an exception.
verify' :: (Parsable a) => Signed_Message -> Either Hetcons_Exception (Verified a)
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
           Left $ Hetcons_Exception_Invalid_Signed_Hash default_Invalid_Signed_Hash {
              invalid_Signed_Hash_signed_hash = signed_hash
             ,invalid_Signed_Hash_explanation = Just $ pack e}
         Nothing ->
           case parse payload of
             (Left  e) -> Left e
             (Right x) -> Right Verified { verified_original = x, verified_signed = signed_message }}

-- | If it's a public crypto key, but not an x509, we return an appropriate Exception
verify' Signed_Message
       {signed_Message_signature =
          Signed_Hash
          {signed_Hash_crypto_id = Just
             Crypto_ID
             {crypto_ID_public_crypto_key = Just
                public_key@(Public_Crypto_Key
                  {public_Crypto_Key_public_crypto_key_x509 = Nothing})}}} -- it's a public crypto key, but not an x509
  = Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
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
  = Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
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
  = Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
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
  = Left $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
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
  = Left $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
             default_No_Supported_Hash_Type_Descriptor_Provided  {
               no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor = Nothing
              ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor = Just sUPPORTED_HASH_TYPE_DESCRIPTOR
              ,no_Supported_Hash_Type_Descriptor_Provided_explanation = Just "No Hash Type Descriptor provided"}



-- | builds a Signed_Message given a signing key, a matching certificate, something serializable, etc.
sign ::(Serialize serialize, X509.Signer signer, DRG gen) =>
       Crypto_ID -> signer -> Signed_Hash_Type_Descriptor -> gen -> serialize -> Either Hetcons_Exception Signed_Message
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
      ;signature <- mapLeft (\e ->(Hetcons_Exception_Invalid_Signed_Hash default_Invalid_Signed_Hash {
                                     invalid_Signed_Hash_explanation = Just $ pack (
                                       "Something went wrong while trying to sign with this key:\n" ++ e)}))
                            $ sign_with_length random_generator private_key serialized_payload
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
  = Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
             default_Descriptor_Does_Not_Match_Public_Crypto_Key {
                descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key_type_descriptor =
                  sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Public_Crypto_Key_public_crypto_key =
                  public_key
               ,descriptor_Does_Not_Match_Public_Crypto_Key_explanation =
                  Just "Alas, I only support X509 Public Crypto Keys at this time."}

-- | If the crypto ID isn't a public key
sign crypto_id@(Crypto_ID {crypto_ID_public_crypto_key = Nothing}) _ _ _ _ =
  Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
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
  = Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
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
  = Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
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
  = Left $ Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided
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
  = Left $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
             default_No_Supported_Hash_Type_Descriptor_Provided {
                no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor =
                  Just hash_type_descriptor
               ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor =
                  Just sUPPORTED_HASH_TYPE_DESCRIPTOR
               ,no_Supported_Hash_Type_Descriptor_Provided_explanation =
                  Just "Alas, I only support SHA2 hashes at this time."}

-- | If the Signed_Hash_Type_Descriptor specifies a Hash, but it's not SHA2
sign _ _ (Signed_Hash_Type_Descriptor {signed_Hash_Type_Descriptor_hash_type_descriptor = Nothing}) _ _ =
  Left $ Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
           default_No_Supported_Hash_Type_Descriptor_Provided  {
             no_Supported_Hash_Type_Descriptor_Provided_offending_hash_type_descriptor = Nothing
            ,no_Supported_Hash_Type_Descriptor_Provided_supported_hash_type_descriptor = Just sUPPORTED_HASH_TYPE_DESCRIPTOR
            ,no_Supported_Hash_Type_Descriptor_Provided_explanation = Just "No Hash Type Descriptor provided"}
