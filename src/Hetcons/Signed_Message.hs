{-# LANGUAGE OverloadedStrings #-}

module Hetcons.Signed_Message
    ( verify
    ) where

import Hetcons.Hetcons_Exception (Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                                                   ,Hetcons_Exception_Unparsable_Hashable_Message
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                                                   ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Invalid_Signed_Hash))

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
                     )

import           Control.Monad.Error    ()
import           Crypto.Hash.Algorithms (SHA224(SHA224)
                                        ,SHA256(SHA256)
                                        ,SHA384(SHA384)
                                        ,SHA512(SHA512))
import           Crypto.Random          (DRG)
import           Data.Either.Combinators(mapLeft)
import           Data.Foldable          (null
                                        ,maximum)
import           Data.HashSet           (HashSet
                                        ,intersection
                                        ,singleton)
import qualified Data.HashSet as HashSet(map)
import           Data.Serialize         (Serialize
                                        ,encodeLazy
                                        ,decodeLazy)
import           Data.Text.Lazy         (pack)
import qualified EasyX509       as X509 (sign
                                        ,verify
                                        ,Signer)

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


verify :: Serialize a => Signed_Message -> Either Hetcons_Exception a
-- In the case where everything's done correctly:
verify Signed_Message
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
           case decodeLazy payload of
             (Left e) ->
               Left $ Hetcons_Exception_Unparsable_Hashable_Message default_Unparsable_Hashable_Message {
                  unparsable_Hashable_Message_message = payload
                 ,unparsable_Hashable_Message_explanation = Just "I was unable to parse this message payload"}
             (Right x) -> Right x}

-- | If it's a public crypto key, but not an x509, we return an appropriate Exception
verify Signed_Message
       {signed_Message_signature =
          signed_hash@Signed_Hash
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
verify Signed_Message
       {signed_Message_signature =
          signed_hash@Signed_Hash
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
verify Signed_Message
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
verify Signed_Message
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
verify Signed_Message
       {signed_Message_signature =
          signed_hash@Signed_Hash
          {signed_Hash_hash_type_descriptor = Nothing}}
  = Left $ Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
             default_Descriptor_Does_Not_Match_Signed_Hash {
                descriptor_Does_Not_Match_Signed_Hash_signed_hash_type_descriptor =
                  sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
               ,descriptor_Does_Not_Match_Signed_Hash_signed_hash =
                  signed_hash
               ,descriptor_Does_Not_Match_Signed_Hash_explanation =
                  Just "Alas, I require a type descriptor in my signed hashes."}



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
