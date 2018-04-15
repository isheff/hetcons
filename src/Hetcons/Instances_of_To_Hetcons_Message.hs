{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The properties of a 1A message,such as how to parse it, extract valuese from it, etc.
module Hetcons.Instances_of_To_Hetcons_Message () where

import Hetcons.Quorums (Monad_Verify_Quorums, verify_quorums )
import Hetcons.Receive_Message (Hetcons_Transaction, get_my_crypto_id, get_my_private_key)
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,To_Hetcons_Message
       ,to_Hetcons_Message
     ,From_Hetcons_Message
       ,from_Hetcons_Message
     ,Recursive_1a(Recursive_1a)
       ,recursive_1a_non_recursive
       ,recursive_1a_filled_in
       ,recursive_1a_value
     ,Parsable
       ,parse
     ,Verified
       ,original)
import Hetcons.Value
    ( Contains_Value(extract_value)
     ,Contains_1a(extract_1a)
     ,Value
    )
import Instances_1a ()

import Charlotte_Types
    ( hetcons_Message_proposals
     ,hetcons_Message_phase_1as
     ,default_Hetcons_Message
     ,signed_Index_index
     ,signed_Index_signature
     ,default_Signed_Index
    )

import Crypto.Random (drgNew)

-- TODO: maybe let's not hard code sUPPORTED_SINGED_HASH_TYPE_DESCRIPTOR in here...
instance {-# OVERLAPPING #-} (Value v) => To_Hetcons_Message Hetcons_Transaction (Recursive_1a v) where
  to_Hetcons_Message r1a = do
    {let non_recursive = recursive_1a_non_recursive r1a
    ;crypto_id <- get_my_crypto_id
    ;private_key <- get_my_private_key
    ;generator <- drgNew
    ;let type_descriptor = sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
    ;signature <- sign crypto_id private_key type_descriptor generator $ encode non_recursive
    ;return default_Hetcons_Message {
        hetcons_Message_proposals = singleton non_recursive
       ,hetcons_Message_phase_1as = singleton default_Signed_Index {
          signed_Index_index = 0
         ,signed_Index_signature = signature
        }
      }
    }