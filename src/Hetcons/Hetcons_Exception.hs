{-# LANGUAGE DeriveGeneric #-}

-- | Defines the type of Exception which can be used in Hetcons, and passed via Thrift
module Hetcons.Hetcons_Exception
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
                      ,Hetcons_Exception_Invalid_Proposal_1a
                      ,Hetcons_Exception_Invalid_Phase_1b
                      ,Hetcons_Exception_Invalid_Phase_2a
                      ,Hetcons_Exception_Invalid_Phase_2b
                      ,Hetcons_Exception_Invalid_Proof_of_Consensus)
    ) where

import Hetcons_Types
    ( No_Supported_Hash_Sha2_Descriptor_Provided
     ,Descriptor_Does_Not_Match_Hash_Sha2
     ,No_Supported_Hash_Sha3_Descriptor_Provided
     ,Descriptor_Does_Not_Match_Hash_Sha3
     ,No_Supported_Hash_Type_Descriptor_Provided
     ,Descriptor_Does_Not_Match_Hash
     ,Invalid_Public_Crypto_Key_X509
     ,Invalid_Public_Crypto_Key_PGP
     ,No_Supported_Public_Crypto_Key_Type_Descriptor_Provided
     ,Descriptor_Does_Not_Match_Public_Crypto_Key
     ,Descriptor_Does_Not_Match_Crypto_ID_Hash
     ,No_Supported_Crypto_ID_Type_Descriptor_Provided
     ,Descriptor_Does_Not_Match_Crypto_ID
     ,Invalid_Signed_Hash
     ,Descriptor_Does_Not_Match_Signed_Hash
     ,Unparsable_Hashable_Message
     ,Invalid_Address
     ,Impossible_Observer_Graph
     ,Invalid_Proposal_1a
     ,Invalid_Phase_1b
     ,Invalid_Phase_2a
     ,Invalid_Phase_2b
     ,Invalid_Proof_of_Consensus )

import Control.Exception
    ( Exception
     ,toException
     ,fromException
     ,displayException )
import Control.Monad ( liftM )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )


-- | This is a wrapper datatype which exists primarily because I don't like Haskell's Control.Exception s, but Thrift does.
--   Yes, this code is ugly.
--   The basic idea is that we have a union type which handles all of the different Exceptions in Hetcons' Thrift.
--   We can throw a Hetcons_Exception, and it is equivalent to throwing the underlying Exception.
--   Likewise, anywhere we can catch an Exception from Thrift, we can catch a Hetcons_Exception.
--   Unlike SomeException, these are only the Exceptions from Thrift, and we can explicitly match for each.
--   In general, a Hetcons_Exceptions behaves pretty much like its underlying Exception from Thrift.
data Hetcons_Exception =
    Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided              No_Supported_Hash_Sha2_Descriptor_Provided
  | Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2                     Descriptor_Does_Not_Match_Hash_Sha2
  | Hetcons_Exception_No_Supported_Hash_Sha3_Descriptor_Provided              No_Supported_Hash_Sha3_Descriptor_Provided
  | Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha3                     Descriptor_Does_Not_Match_Hash_Sha3
  | Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided              No_Supported_Hash_Type_Descriptor_Provided
  | Hetcons_Exception_Descriptor_Does_Not_Match_Hash                          Descriptor_Does_Not_Match_Hash
  | Hetcons_Exception_Invalid_Public_Crypto_Key_X509                          Invalid_Public_Crypto_Key_X509
  | Hetcons_Exception_Invalid_Public_Crypto_Key_PGP                           Invalid_Public_Crypto_Key_PGP
  | Hetcons_Exception_No_Supported_Public_Crypto_Key_Type_Descriptor_Provided No_Supported_Public_Crypto_Key_Type_Descriptor_Provided
  | Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key             Descriptor_Does_Not_Match_Public_Crypto_Key
  | Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID_Hash                Descriptor_Does_Not_Match_Crypto_ID_Hash
  | Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided         No_Supported_Crypto_ID_Type_Descriptor_Provided
  | Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID                     Descriptor_Does_Not_Match_Crypto_ID
  | Hetcons_Exception_Invalid_Signed_Hash                                     Invalid_Signed_Hash
  | Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash                   Descriptor_Does_Not_Match_Signed_Hash
  | Hetcons_Exception_Unparsable_Hashable_Message                             Unparsable_Hashable_Message
  | Hetcons_Exception_Invalid_Address                                         Invalid_Address
  | Hetcons_Exception_Impossible_Observer_Graph                               Impossible_Observer_Graph
  | Hetcons_Exception_Invalid_Proposal_1a                                     Invalid_Proposal_1a
  | Hetcons_Exception_Invalid_Phase_1b                                        Invalid_Phase_1b
  | Hetcons_Exception_Invalid_Phase_2a                                        Invalid_Phase_2a
  | Hetcons_Exception_Invalid_Phase_2b                                        Invalid_Phase_2b
  | Hetcons_Exception_Invalid_Proof_of_Consensus                              Invalid_Proof_of_Consensus
  deriving (Show,Eq,Generic,Typeable)

instance Exception Hetcons_Exception where
  toException (Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided x) = toException x
  toException (Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2 x) = toException x
  toException (Hetcons_Exception_No_Supported_Hash_Sha3_Descriptor_Provided x) = toException x
  toException (Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha3 x) = toException x
  toException (Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided x) = toException x
  toException (Hetcons_Exception_Descriptor_Does_Not_Match_Hash x) = toException x
  toException (Hetcons_Exception_Invalid_Public_Crypto_Key_X509 x) = toException x
  toException (Hetcons_Exception_Invalid_Public_Crypto_Key_PGP x) = toException x
  toException (Hetcons_Exception_No_Supported_Public_Crypto_Key_Type_Descriptor_Provided x) = toException x
  toException (Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key x) = toException x
  toException (Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID_Hash x) = toException x
  toException (Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided x) = toException x
  toException (Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID x) = toException x
  toException (Hetcons_Exception_Invalid_Signed_Hash x) = toException x
  toException (Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash x) = toException x
  toException (Hetcons_Exception_Unparsable_Hashable_Message x) = toException x
  toException (Hetcons_Exception_Invalid_Address x) = toException x
  toException (Hetcons_Exception_Impossible_Observer_Graph x) = toException x
  toException (Hetcons_Exception_Invalid_Proposal_1a x) = toException x
  toException (Hetcons_Exception_Invalid_Phase_1b x) = toException x
  toException (Hetcons_Exception_Invalid_Phase_2a x) = toException x
  toException (Hetcons_Exception_Invalid_Phase_2b x) = toException x
  toException (Hetcons_Exception_Invalid_Proof_of_Consensus x) = toException x
  displayException (Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided x) = displayException x
  displayException (Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2 x) = displayException x
  displayException (Hetcons_Exception_No_Supported_Hash_Sha3_Descriptor_Provided x) = displayException x
  displayException (Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha3 x) = displayException x
  displayException (Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided x) = displayException x
  displayException (Hetcons_Exception_Descriptor_Does_Not_Match_Hash x) = displayException x
  displayException (Hetcons_Exception_Invalid_Public_Crypto_Key_X509 x) = displayException x
  displayException (Hetcons_Exception_Invalid_Public_Crypto_Key_PGP x) = displayException x
  displayException (Hetcons_Exception_No_Supported_Public_Crypto_Key_Type_Descriptor_Provided x) = displayException x
  displayException (Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key x) = displayException x
  displayException (Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID_Hash x) = displayException x
  displayException (Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided x) = displayException x
  displayException (Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID x) = displayException x
  displayException (Hetcons_Exception_Invalid_Signed_Hash x) = displayException x
  displayException (Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash x) = displayException x
  displayException (Hetcons_Exception_Unparsable_Hashable_Message x) = displayException x
  displayException (Hetcons_Exception_Invalid_Address x) = displayException x
  displayException (Hetcons_Exception_Impossible_Observer_Graph x) = displayException x
  displayException (Hetcons_Exception_Invalid_Proposal_1a x) = displayException x
  displayException (Hetcons_Exception_Invalid_Phase_1b x) = displayException x
  displayException (Hetcons_Exception_Invalid_Phase_2a x) = displayException x
  displayException (Hetcons_Exception_Invalid_Phase_2b x) = displayException x
  displayException (Hetcons_Exception_Invalid_Proof_of_Consensus x) = displayException x
  fromException x = foldr (\g old ->let new = g x in if new == Nothing then old else new)
                          Nothing
                          -- The below code is even dumber than the rest of this file, but it's a "different fromException" every time.
                          [(liftM Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided) . fromException
                          ,(liftM Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha2) . fromException
                          ,(liftM Hetcons_Exception_No_Supported_Hash_Sha3_Descriptor_Provided) . fromException
                          ,(liftM Hetcons_Exception_Descriptor_Does_Not_Match_Hash_Sha3) . fromException
                          ,(liftM Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided) . fromException
                          ,(liftM Hetcons_Exception_Descriptor_Does_Not_Match_Hash) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Public_Crypto_Key_X509) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Public_Crypto_Key_PGP) . fromException
                          ,(liftM Hetcons_Exception_No_Supported_Public_Crypto_Key_Type_Descriptor_Provided) . fromException
                          ,(liftM Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key) . fromException
                          ,(liftM Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID_Hash) . fromException
                          ,(liftM Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided) . fromException
                          ,(liftM Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Signed_Hash) . fromException
                          ,(liftM Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash) . fromException
                          ,(liftM Hetcons_Exception_Unparsable_Hashable_Message) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Address) . fromException
                          ,(liftM Hetcons_Exception_Impossible_Observer_Graph) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Proposal_1a) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Phase_1b) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Phase_2a) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Phase_2b) . fromException
                          ,(liftM Hetcons_Exception_Invalid_Proof_of_Consensus) . fromException]

