{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A class of function input types which can be memoized, and a function which produces a memoized function version
module Hetcons.Memoize (memoize) where

import Hetcons_Types

import Data.ByteString.Lazy (ByteString, pack, unpack)
import Data.HashSet
import Data.MemoCombinators
import Data.Serialize
import Data.Word
import GHC.Word

bytes :: Memo [Word8]
bytes = list bits

bytestrings :: Memo ByteString
bytestrings = wrap pack unpack bytes


to_signed_message payload sig cert =
  default_Signed_Message  {
    signed_Message_payload = payload
   ,signed_Message_signature =
     default_Signed_Hash  {
       signed_Hash_signature = sig
      ,signed_Hash_hash_type_descriptor = Just
        default_Hash_Type_Descriptor  {
          hash_Type_Descriptor_sha2 = Just $ singleton 64}
      ,signed_Hash_crypto_id = Just
         default_Crypto_ID  {
           crypto_ID_public_crypto_key = Just
             default_Public_Crypto_Key  {
               public_Crypto_Key_public_crypto_key_x509 = Just cert}}}}

from_signed_message (
  Signed_Message  {
    signed_Message_payload = payload
   ,signed_Message_signature =
     Signed_Hash  {
       signed_Hash_signature = sig
      ,signed_Hash_hash_type_descriptor = Just
        Hash_Type_Descriptor  {
          hash_Type_Descriptor_sha2 = Just hash_sizes}
      ,signed_Hash_crypto_id = Just
         Crypto_ID  {
           crypto_ID_public_crypto_key = Just
             Public_Crypto_Key  {
               public_Crypto_Key_public_crypto_key_x509 = Just cert}}}})
  = if member 64 hash_sizes
       then (payload, sig, cert)
       else error "not a valid hash size"



memoize ::  (Signed_Message -> b) -> (Signed_Message -> b)
memoize f s = let m = memo3 bytestrings bytestrings bytestrings (\x y z -> f $ to_signed_message x y z)
                  (x,y,z) = from_signed_message s
               in m x y z
