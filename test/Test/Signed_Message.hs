module Test.Signed_Message (signed_message_tests) where

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

import Hetcons_Types (default_No_Supported_Hash_Sha2_Descriptor_Provided
                     ,default_Descriptor_Does_Not_Match_Hash_Sha2)

import Test.HUnit (Test(TestList,
                        TestLabel,
                        TestCase)
                  ,assertEqual
                  ,assertBool)

signed_message_tests = TestList []
