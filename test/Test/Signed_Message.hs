{-# LANGUAGE OverloadedStrings #-}
module Test.Signed_Message (signed_message_tests) where

import Hetcons.Hetcons_Exception (Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                                                   ,Hetcons_Exception_Unparsable_Hashable_Message
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                                                   ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                                                   ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                                                   ,Hetcons_Exception_Invalid_Signed_Hash))
import Hetcons.Signed_Message (sign
                              ,verify)


import Hetcons_Consts(sUPPORTED_HASH_SHA2_DESCRIPTOR
                     ,sUPPORTED_CRYPTO_ID_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
                     ,sUPPORTED_PUBLIC_CRYPTO_KEY_TYPE_DESCRIPTOR)

import Hetcons_Types (default_No_Supported_Hash_Sha2_Descriptor_Provided
                     ,crypto_ID_public_crypto_key
                     ,default_Public_Crypto_Key
                     ,default_Crypto_ID
                     ,public_Crypto_Key_public_crypto_key_x509
                     ,signed_Message_payload
                     ,default_Descriptor_Does_Not_Match_Hash_Sha2)

import Crypto.Random (getSystemDRG)
import qualified Data.ByteString.Lazy as ByteString (readFile, concat, take, drop, singleton)
import Data.Either.Combinators (mapRight)
import           Data.Serialize         (decodeLazy)
import Test.HUnit (Test(TestList,
                        TestLabel,
                        TestCase)
                  ,assertEqual
                  ,assertBool)

signed_message_tests = TestList [
  TestLabel "verify that we can sign and verify a thing" (
    TestCase (
      do { gen <- getSystemDRG
         ; cert <- ByteString.readFile "test/cert.pem"
         ; private <- ByteString.readFile "test/key.pem"
         ; let message = 1337 :: Integer
         ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                              Just (default_Public_Crypto_Key {
                                public_Crypto_Key_public_crypto_key_x509 = Just cert})}
         ; let signed = sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen message
         ; assertEqual "signed value came out wrong" (Right $ Right message) $ mapRight (decodeLazy.signed_Message_payload) signed
         ; let signed_decoded = mapRight verify signed
         ; assertEqual "could not verify" (Right $ Right message) signed_decoded
         ; return ()}))]

