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

import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Signed_Message (Verified
                                 ,signed
                                 ,original
                              ,sign
                              ,verify
                              ,Recursive_1a
                              ,Recursive_2a (Recursive_2a)
                              ,Recursive_2b (Recursive_2b)
                              ,recursive_1b_proposal
                              ,recursive_1b_conflicting_phase2as
                              ,Parsable
                              ,non_recursive)


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
                     ,default_Descriptor_Does_Not_Match_Hash_Sha2
                     ,Proposal_1a
                        ,proposal_1a_value
                        ,proposal_1a_timestamp
                        ,default_Proposal_1a
                        ,proposal_1a_observers
                     ,Observers
                        ,default_Observers
                        ,observers_observer_quorums
                     ,Value
                        ,value_value_payload
                        ,value_slot
                        ,default_Value
                     ,Phase_1b
                        ,phase_1b_proposal
                        ,phase_1b_conflicting_phase2as
                        ,default_Phase_1b
                     ,Phase_2a
                        ,phase_2a_phase_1bs
                        ,default_Phase_2a
                     ,Phase_2b
                        ,phase_2b_phase_1bs
                        ,default_Phase_2b
                     ,participant_ID_address
                     ,address_host_address
                     ,host_Address_dns_name
                     ,address_port_number
                     ,participant_ID_crypto_id
                     ,default_Participant_ID
                     ,default_Address
                     ,default_Host_Address
                     )

import           Control.Monad (join)
import           Control.Monad.Except   (runExceptT)
import Control.Monad.Trans.Except (except)
import Crypto.Random (getSystemDRG, DRG, withDRG)
import qualified Data.ByteString.Lazy as ByteString (readFile, concat, take, drop, singleton, index)
import Data.Either.Combinators (isLeft)
import Data.Either.Combinators (mapRight)
import           Data.HashMap.Lazy           (empty)
import           Data.HashSet           (fromList,toList)
import Data.List (head)
import           Data.Serialize         (Serialize
                                           ,decodeLazy)
import Test.HUnit (Test(TestList,
                        TestLabel,
                        TestCase)
                  ,assertEqual
                  ,assertBool)
import Data.HashMap.Strict (singleton)
import           Data.Text.Lazy         (pack)


doubleGen :: (DRG g) => g -> (g,g)
doubleGen g = withDRG g (return g)

listGen :: (DRG g) => g -> [g]
listGen g = g:(listGen (snd (withDRG g (return ()))))


sample_payload :: Integer
sample_payload = 1337

sample_message :: IO (Either Hetcons_Exception Signed_Message)
sample_message = sample_sign sample_payload


sample_sign :: (Serialize a) => a -> IO (Either Hetcons_Exception Signed_Message)
sample_sign payload =
  do { gen <- getSystemDRG
     ; cert <- ByteString.readFile "test/cert.pem"
     ; private <- ByteString.readFile "test/key.pem"
     ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                          Just (default_Public_Crypto_Key {
                            public_Crypto_Key_public_crypto_key_x509 = Just cert})}
     ; return $ sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen payload}

sample_id cert =
  default_Participant_ID  {
    participant_ID_address =
      default_Address  {
        address_host_address =
          default_Host_Address  {
            host_Address_dns_name = Just $ pack "localhost"}
       ,address_port_number = 8976}
   ,participant_ID_crypto_id =
      default_Crypto_ID {
        crypto_ID_public_crypto_key =
          Just (default_Public_Crypto_Key {
                  public_Crypto_Key_public_crypto_key_x509 = Just cert})}}

-- sample_1a :: Proposal_1a
sample_1a cert = default_Proposal_1a {
   proposal_1a_value = default_Value {
                          value_value_payload = ByteString.singleton 42
                         ,value_slot = 6}
  ,proposal_1a_timestamp = 1111111
  ,proposal_1a_observers = Just default_Observers {
     observers_observer_quorums = Just $ singleton (sample_id cert) (fromList [ fromList [sample_id cert]])}}



deStupidify :: (Monad m) => Either a (m b) -> (m (Either a b))
deStupidify (Left  x) = return (Left x)
deStupidify (Right x) = do { y <- x
                           ; return (Right y)}


signed_message_tests = TestList [
   TestLabel "verify that we can sign a thing" (
     TestCase (
       do { signed <- sample_message
          ; assertEqual "signed value came out wrong" (Right $ Right sample_payload) $ mapRight (decodeLazy.signed_Message_payload) signed
          ; return ()}))
  ,TestLabel "verify that we can sign and verify a thing" (
     TestCase (
       do { signed <- sample_message
          ; assertEqual "could not verify" (Right $ Right sample_payload) $ mapRight ((mapRight original).verify) signed
          ; return ()}))
  ,TestLabel "verify that we can't verify incorrect signatures" (
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
               ((mapRight verify borked_message) :: Either Hetcons_Exception (Either Hetcons_Exception (Verified Integer)))
          ; return ()}))
  ,TestLabel "verify that we can sign and parse a 1A message" (
     TestCase (
       do { cert <- ByteString.readFile "test/cert.pem"
          ; signed <- sample_sign $ sample_1a cert
          ; assertEqual "failed to verify a signed proposal_1a" (Right $ Right $ sample_1a cert)
               $ mapRight ((mapRight ((non_recursive :: Recursive_1a -> Proposal_1a).original)).verify) signed
          ; return ()}))
  ,TestLabel "verify that we can sign and parse a 1B message" (
     TestCase (
       do { cert <- ByteString.readFile "test/cert.pem"
          ; signed_1a <- sample_sign $ sample_1a cert
          ; let payload = (mapRight (\x -> default_Phase_1b { phase_1b_proposal = x }) signed_1a) :: Either Hetcons_Exception Phase_1b
          ; signed <- fmap join $ deStupidify $ mapRight sample_sign payload
          ; assertEqual "failed to verify a signed phase_1b" (Right $ sample_1a cert)
               $ mapRight (non_recursive.original.recursive_1b_proposal.original) $ join $ mapRight (verify) signed
          ; return ()}))
  ,TestLabel "verify that we can sign and parse a 2A message" (
     TestCase (
       do { gen <- getSystemDRG
          ; let l_gen = listGen gen
          ; cert <- ByteString.readFile "test/cert.pem"
          ; private <- ByteString.readFile "test/key.pem"
          ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                               Just (default_Public_Crypto_Key {
                                 public_Crypto_Key_public_crypto_key_x509 = Just cert})}
          ; let gen_sign_1a = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Proposal_1a -> (Either Hetcons_Exception Signed_Message)
          ; let gen_sign_1b = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Phase_1b -> (Either Hetcons_Exception Signed_Message)
          ; let gen_sign_2a = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Phase_2a -> (Either Hetcons_Exception Signed_Message)
          ; cert <- ByteString.readFile "test/cert.pem"
          ; let result = do { signed_1a <- gen_sign_1a (l_gen!!0) $ sample_1a cert
                            ; let phase_1b = default_Phase_1b { phase_1b_proposal = signed_1a }
                            ; signed_1b_1 <- gen_sign_1b (l_gen!!1) phase_1b
                            ; signed_1b_2 <- gen_sign_1b (l_gen!!2) phase_1b
                            ; let phase_2a = default_Phase_2a {phase_2a_phase_1bs = fromList [signed_1b_1, signed_1b_2]}
                            ; signed <- gen_sign_2a (l_gen!!3) phase_2a
                            ; verified <- verify signed
                            ; return ((original.recursive_1b_proposal.original.head.toList.(\(Recursive_2a x) -> x).original) verified)
                            }
          ; assertEqual "failed to verify a signed phase_2a" (Right $ sample_1a cert) $ mapRight non_recursive result
          ; return ()}))
  ,TestLabel "verify that we can sign and parse a 1B message with 2A messages inside of it" (
     TestCase (
       do { gen <- getSystemDRG
          ; let l_gen = listGen gen
          ; cert <- ByteString.readFile "test/cert.pem"
          ; private <- ByteString.readFile "test/key.pem"
          ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                               Just (default_Public_Crypto_Key {
                                 public_Crypto_Key_public_crypto_key_x509 = Just cert})}
          ; let gen_sign_1a = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Proposal_1a -> (Either Hetcons_Exception Signed_Message)
          ; let gen_sign_1b = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Phase_1b -> (Either Hetcons_Exception Signed_Message)
          ; let gen_sign_2a = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Phase_2a -> (Either Hetcons_Exception Signed_Message)
          ; cert <- ByteString.readFile "test/cert.pem"
          ; let result = do { signed_1a <- gen_sign_1a (l_gen!!0) $ sample_1a cert
                            ; let phase_1b = default_Phase_1b { phase_1b_proposal = signed_1a }
                            ; signed_1b_1 <- gen_sign_1b (l_gen!!1) phase_1b
                            ; signed_1b_2 <- gen_sign_1b (l_gen!!2) phase_1b
                            ; let phase_2a = default_Phase_2a {phase_2a_phase_1bs = fromList [signed_1b_1, signed_1b_2]}
                            ; signed_2a_1 <- gen_sign_2a (l_gen!!3) default_Phase_2a {phase_2a_phase_1bs = fromList [signed_1b_1, signed_1b_2]}
                            ; signed_2a_2 <- gen_sign_2a (l_gen!!4) default_Phase_2a {phase_2a_phase_1bs = fromList [signed_1b_1]}
                            ; signed_1a' <- gen_sign_1a (l_gen!!6) $ (sample_1a cert) {
                                                                                      proposal_1a_value = default_Value {
                                                                                                             value_value_payload = ByteString.singleton 43
                                                                                                            ,value_slot = 6}}
                            ; signed <- gen_sign_1b (l_gen!!5) phase_1b {phase_1b_proposal = signed_1a'
                                                                       ,phase_1b_conflicting_phase2as = fromList [signed_2a_1, signed_2a_2]}
                            ; verified <- verify signed
                            ; return ((original.recursive_1b_proposal.original.head.toList.(\(Recursive_2a x) -> x).
                                  original.head.toList.recursive_1b_conflicting_phase2as.original) verified)
                            }
          ; assertEqual "failed to verify a signed phase_1a with 2b messages inside of it" (Right $ sample_1a cert) $ mapRight non_recursive result
          ; return ()}))
  ,TestLabel "verify that we can sign and parse a 2B message" (
     TestCase (
       do { gen <- getSystemDRG
          ; let l_gen = listGen gen
          ; cert <- ByteString.readFile "test/cert.pem"
          ; private <- ByteString.readFile "test/key.pem"
          ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                               Just (default_Public_Crypto_Key {
                                 public_Crypto_Key_public_crypto_key_x509 = Just cert})}
          ; let gen_sign_1a = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Proposal_1a -> (Either Hetcons_Exception Signed_Message)
          ; let gen_sign_1b = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Phase_1b -> (Either Hetcons_Exception Signed_Message)
          ; let gen_sign_2b = (sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR ) ::
                  (DRG gen) => gen -> Phase_2b -> (Either Hetcons_Exception Signed_Message)
          ; cert <- ByteString.readFile "test/cert.pem"
          ; let result = do { signed_1a <- gen_sign_1a (l_gen!!0) $ sample_1a cert
                            ; let phase_1b = default_Phase_1b { phase_1b_proposal = signed_1a }
                            ; signed_1b_1 <- gen_sign_1b (l_gen!!1) phase_1b
                            ; signed_1b_2 <- gen_sign_1b (l_gen!!2) phase_1b
                            ; let phase_2b = default_Phase_2b {phase_2b_phase_1bs = fromList [signed_1b_1, signed_1b_2]}
                            ; signed <- gen_sign_2b (l_gen!!3) phase_2b
                            ; verified <- verify signed
                            ; return ((original.recursive_1b_proposal.original.head.toList.(\(Recursive_2b x) -> x).original) verified)
                            }
          ; assertEqual "failed to verify a signed phase_2b" (Right $ sample_1a cert) $ mapRight non_recursive result
          ; return ()}))

  ]
