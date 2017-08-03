module Test.Hetcons_Exception (hetcons_exception_tests) where

import Hetcons.Hetcons_Exception
    (Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2
                      ,Hetcons_Exception_No_Supported_Hash_Sha3_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha3
                      ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Hash
                      ,Hetcons_Exception_Invalid_Public_Crypto_Key_X509
                      ,Hetcons_Exception_Invalid_Public_Crypto_Key_PGP
                      ,Hetcons_Exception_No_Supported_Public_Crypto_Key_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID_Hash
                      ,Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                      ,Hetcons_Exception_Invalid_Signed_Hash
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                      ,Hetcons_Exception_Unparsable_Hashable_Message
                      ,Hetcons_Exception_Invalid_Address
                      ,Hetcons_Exception_Impossible_Observer_Graph
                      ,Hetcons_Exception_Invalid_Proposal_1a))
import Test.Util ()

import Hetcons_Types (default_No_Supported_Hash_Sha2_Descriptor_Provided
                     ,default_Descriptor_Does_Not_Match_Hash_Sha2)

import Control.Exception (throw,
                          catch)
import Test.HUnit (Test(TestList,
                        TestLabel,
                        TestCase)
                  ,assertEqual
                  )

hetcons_exception_tests = TestList [
   TestLabel "Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided can be thrown and caught as underlying Exception" (
     TestCase (catch (throw $ Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided default_No_Supported_Hash_Sha2_Descriptor_Provided)
                     (assertEqual "caught Exception fails to match thrown one" default_No_Supported_Hash_Sha2_Descriptor_Provided)))
  ,TestLabel "Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2 can be thrown and caught as underlying Exception" (
     TestCase (catch (throw $ Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2 default_Descriptor_Does_Not_Match_Hash_Sha2)
                     (assertEqual "caught Exception fails to match thrown one" default_Descriptor_Does_Not_Match_Hash_Sha2)))

  ,TestLabel "No_Supported_Hash_Sha2_Descriptor_Provided can be thrown and caught as Hetcons_Exception" (
     TestCase (catch (throw $ default_No_Supported_Hash_Sha2_Descriptor_Provided)
                     (assertEqual "caught Exception fails to match thrown one"
                        $ Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided default_No_Supported_Hash_Sha2_Descriptor_Provided)))
  ,TestLabel "Descriptor_Does_Not_Match_Hash_Sha2 can be thrown and caught as Hetcons_Exception" (
     TestCase (catch (throw $ default_Descriptor_Does_Not_Match_Hash_Sha2)
                     (assertEqual "caught Exception fails to match thrown one"
                        $ Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2 default_Descriptor_Does_Not_Match_Hash_Sha2)))]
