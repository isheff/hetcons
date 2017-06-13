{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Receive_Message (Receive_Message{-- , run_receive_message, receive --}) where

import Hetcons.Contains_Value (
      Contains_Value
        ,extract_value
    , Contains_1a
        ,extract_1a
        ,extract_observer_quorums
    , Ballot
        ,extract_ballot
    ,Contains_1bs
        ,extract_1bs
    )
import Hetcons.Hetcons_Exception (
     Hetcons_Exception(Hetcons_Exception_No_Supported_Hash_Sha2_Descriptor_Provided
                      ,Hetcons_Exception_No_Supported_Hash_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Public_Crypto_Key
                      ,Hetcons_Exception_No_Supported_Crypto_ID_Type_Descriptor_Provided
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Crypto_ID
                      ,Hetcons_Exception_Invalid_Signed_Hash
                      ,Hetcons_Exception_Descriptor_Does_Not_Match_Signed_Hash
                      ,Hetcons_Exception_Unparsable_Hashable_Message
                      ,Hetcons_Exception_Invalid_Proof_of_Consensus)
    )
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Quorums (verify_quorums)
import Hetcons.Signed_Message
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
       ,recursive_1b_proposal
       ,recursive_1b_conflicting_phase2as
    , Recursive_2a (Recursive_2a )
    , Recursive_2b (Recursive_2b )
    , Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
    , Parsable
       ,parse
    )
import Hetcons.Hetcons_State (Hetcons_State, start_Hetcons_State)

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
                     ,Participant_ID
                        ,participant_ID_crypto_id
                     )

import           Crypto.Hash.Algorithms (SHA224(SHA224)
                                        ,SHA256(SHA256)
                                        ,SHA384(SHA384)
                                        ,SHA512(SHA512))
import           Control.Monad          (liftM, liftM2, mapM_)
import           Control.Monad.Except   (throwError)
import           Control.Monad.Trans.Either (EitherT, runEitherT)
import           Control.Monad.State    (State, runState, get, put, state)
import           Crypto.Random          (DRG)
import           Data.ByteString.Lazy   (ByteString, unpack)
import           Data.Either.Combinators(mapLeft)
import           Data.Foldable          (null
                                        ,any
                                        ,length
                                        ,maximum)
import           GHC.Generics           (Generic)
import           Data.Hashable          (Hashable
                                        ,hashWithSalt)
import           Data.HashMap.Strict    (keys, (!))
import           Data.HashSet           (HashSet
                                        ,member
                                        ,unions
                                        ,union
                                        ,intersection
                                        ,toList
                                        ,fromList
                                        ,singleton)
import qualified Data.HashSet as HashSet(map, empty, foldr, filter)
import           Data.List              (head)
import           Data.Typeable          (Typeable )
import           Data.Serialize         (Serialize
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


data Receive_Message_State = Receive_Message_State {
  sent_1as :: HashSet Proposal_1a
 ,sent_1bs :: HashSet Phase_1b
 ,sent_2as :: HashSet Phase_2a
 ,sent_2bs :: HashSet Phase_2b
 ,sent_Proof_of_Consensus :: HashSet Proof_of_Consensus
 ,hetcons_state :: Hetcons_State
}

newtype Receive_Message a = Receive_Message {unwrap :: (EitherT Hetcons_Exception (State Receive_Message_State) a)}

instance Functor Receive_Message where
  fmap ab ma = ma >>= (return . ab) -- this is generally true of Monads

instance Applicative Receive_Message where
  pure = Receive_Message . pure
  f <*> x = Receive_Message $ (unwrap f) <*> (unwrap x)

instance Monad Receive_Message where
  x >>= f = Receive_Message $ (unwrap x) >>= (unwrap . f)

  -- | fail Should really take advantage of Hetcons_Exception, but there's no specific exception that works here, since it's a very general failing...

run_Receive_Message :: (Receive_Message a) -> Receive_Message_State ->  Either Hetcons_Exception (a, Receive_Message_State)
run_Receive_Message x s = case (runState (runEitherT $ unwrap x) s) of
                            (Left  e, _) -> Left  e
                            (Right x, s) -> Right (x,s)

get_Receive_Message_State :: Receive_Message Receive_Message_State
get_Receive_Message_State = Receive_Message get

put_Receive_Message_State :: Receive_Message_State -> Receive_Message ()
put_Receive_Message_State = Receive_Message . put

update_Receive_Message_State :: (Receive_Message_State -> (a, Receive_Message_State)) -> Receive_Message a
update_Receive_Message_State = Receive_Message . state

get_state :: Receive_Message Hetcons_State
get_state = get_Receive_Message_State >>= (return . hetcons_state)

put_state :: Hetcons_State -> Receive_Message ()
put_state s = update_Receive_Message_State (\x -> ((), x{hetcons_state = s}))

update_state :: (Hetcons_State -> (a, Hetcons_State)) -> Receive_Message a
update_state f = update_Receive_Message_State (\x -> let (y,z) = f $ hetcons_state x
                                                      in (y,x{hetcons_state = z}))

add_sent_1a :: Proposal_1a -> Receive_Message ()
add_sent_1a p = update_Receive_Message_State (\x -> ((),x{sent_1as = insert p $ sent_1as x}))

add_sent_1b :: Phase_1b -> Receive_Message ()
add_sent_1b p = update_Receive_Message_State (\x -> ((),x{sent_1bs = insert p $ sent_1bs x}))

add_sent_2a :: Phase_2a -> Receive_Message ()
add_sent_2a p = update_Receive_Message_State (\x -> ((),x{sent_2as = insert p $ sent_2as x}))

add_sent_2b :: Phase_2b -> Receive_Message ()
add_sent_2b p = update_Receive_Message_State (\x -> ((),x{sent_2bs = insert p $ sent_2bs x}))

add_sent_Proof_of_Consensus :: Proof_of_Consensus -> Receive_Message ()
add_sent_Proof_of_Consensus p = update_Receive_Message_State (\x -> ((),x{sent_Proof_of_Consensus = insert p $ sent_Proof_of_Consensus x}))



class Receivable' a where
  receive' :: a -> Receive_Message ()

class Receivable a where
  receive :: a -> Receive_Message ()

instance {-# OVERLAPPABLE #-} (Receivable' a) => (Receivable a) where
  receive = receive'

instance {-# OVERLAPPING #-} (Contains_1bs a, (Receivable' (Verified a))) => (Receivable (Verified a)) where
  receive x = (mapM_ receive $ toList $ extract_1bs $ original x) >> (receive' x)




class Sendable a where
  send :: a -> Receive_Message ()
