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

import Hetcons_Types (Signed_Message
                        ,signed_Hash_signature
                        ,signed_Message_signature
                     ,default_No_Supported_Hash_Sha2_Descriptor_Provided
                     ,crypto_ID_public_crypto_key
                     ,default_Public_Crypto_Key
                     ,default_Crypto_ID
                     ,public_Crypto_Key_public_crypto_key_x509
                     ,signed_Message_payload
                     ,default_Descriptor_Does_Not_Match_Hash_Sha2)

import Crypto.Random (getSystemDRG)
import qualified Data.ByteString.Lazy as ByteString (readFile, concat, take, drop, singleton, index)
import Data.Either.Combinators (isLeft)
import Data.Either.Combinators (mapRight)
import           Data.Serialize         (decodeLazy)
import Test.HUnit (Test(TestList,
                        TestLabel,
                        TestCase)
                  ,assertEqual
                  ,assertBool)

sample_payload :: Integer
sample_payload = 1337

sample_message :: IO (Either Hetcons_Exception Signed_Message)
sample_message =
  do { gen <- getSystemDRG
     ; cert <- ByteString.readFile "test/cert.pem"
     ; private <- ByteString.readFile "test/key.pem"
     ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert})}
     ; return $ sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen sample_payload}


signed_message_tests = TestList [
   TestLabel "verify that we can sign a thing" (
     TestCase (
       do { signed <- sample_message
          ; assertEqual "signed value came out wrong" (Right $ Right sample_payload) $ mapRight (decodeLazy.signed_Message_payload) signed
          ; return ()}))
  ,TestLabel "verify that we can sign and verify a thing" (
     TestCase (
       do { signed <- sample_message
          ; assertEqual "could not verify" (Right $ Right sample_payload) $ mapRight verify signed
          ; return ()}))
  ,TestLabel "verify that we can sign and verify a thing" (
     TestCase (
       do { signed <- sample_message
          ; let borked_message = mapRight (\x -> x {
                  signed_Message_signature =
                    ((signed_Message_signature x) {
                        signed_Hash_signature =
                          let s = (signed_Hash_signature $ signed_Message_signature x)
                           in ByteString.concat [ByteString.take 42 s
                                                ,ByteString.singleton ( 1 + (ByteString.index s 42))
                                                ,ByteString.drop 43 s]
                      })}) signed
          ; assertEqual "verified a signature which should not have worked" (Right True) $ mapRight isLeft
               ((mapRight verify borked_message) :: Either Hetcons_Exception (Either Hetcons_Exception Integer))
          ; return ()}))
  ]
