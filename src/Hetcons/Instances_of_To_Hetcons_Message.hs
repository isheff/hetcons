{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The properties of a 1A message,such as how to parse it, extract valuese from it, etc.
module Hetcons.Instances_of_To_Hetcons_Message () where

import Hetcons.Hetcons_State(Hetcons_State)
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
     ,Recursive_1b(Recursive_1b)
       ,recursive_1b_conflicting_phase2as
       ,recursive_1b_proposal
     ,Recursive_2a(Recursive_2a)
     ,Recursive_2b(Recursive_2b)
     ,Recursive_Proof_of_Consensus(Recursive_Proof_of_Consensus)
     ,sign
     ,signature_bytestring
     ,Parsable
       ,parse
     ,Verified
       ,original
     )
import Hetcons.Value
    ( Contains_Value(extract_value)
     ,Contains_1a(extract_1a)
     ,Value
    )
import Hetcons.Instances_1a ()

import Charlotte_Consts (sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR)
import Charlotte_Types
    ( Hetcons_Message(Hetcons_Message)
       ,hetcons_Message_proposals
       ,hetcons_Message_phase_1as
       ,hetcons_Message_phase_1bs
       ,hetcons_Message_phase_2as
       ,hetcons_Message_index
       ,default_Hetcons_Message
     ,signed_Index_index
     ,signed_Index_signature
     ,default_Signed_Index
     ,phase_1b_Indices_index_1a
     ,phase_1b_Indices_indices_2a
     ,phase_1b_Indices_signature
     ,default_Phase_1b_Indices
     ,signed_Indices_indices
     ,signed_Indices_signature
     ,default_Signed_Indices
     ,signed_Hash_signature
    )

import Crypto.Random (drgNew)
import qualified Data.ByteString.Lazy as ByteString (concat)
import Data.Foldable (foldr, toList)
import Data.Hashable ( Hashable, hashWithSalt )
import Data.HashMap.Lazy ( HashMap, insert )
import qualified Data.HashMap.Lazy as HashMap (fromList, lookup)
import qualified Data.HashSet as HashSet (fromList, map)
import Data.List (reverse, sort, sortOn)
import Data.Vector (Vector, (++), singleton, (!))
import qualified Data.Vector as Vector (fromList, empty, singleton, length)
import Prelude hiding ((++), foldr)

-- Janky way to disable debug statemetns instead of import Control.Monad.Logger.CallStack ( logDebugSH )
logDebugSH _ = return () 

-- TODO: maybe let's not hard code sUPPORTED_SINGED_HASH_TYPE_DESCRIPTOR in here...
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v) => To_Hetcons_Message (Hetcons_Transaction s v) (Recursive_1a v) where
  to_Hetcons_Message r1a = do
    {logDebugSH "to_Hetcons_Message Recursive_1a"
    ;let non_recursive = recursive_1a_non_recursive r1a
    ;crypto_id <- get_my_crypto_id
    ;private_key <- get_my_private_key
    ;generator <- drgNew
    ;let type_descriptor = sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
    ;signature <- sign crypto_id private_key type_descriptor generator $ encode non_recursive
    ;return default_Hetcons_Message {
        hetcons_Message_index = 0
       ,hetcons_Message_proposals = singleton non_recursive
       ,hetcons_Message_phase_1as = singleton default_Signed_Index {
          signed_Index_index = 0
         ,signed_Index_signature = signature
        }
      }
    }

-- TODO: maybe let's not hard code sUPPORTED_SINGED_HASH_TYPE_DESCRIPTOR in here...
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v) => To_Hetcons_Message (Hetcons_Transaction s v) (Recursive_1b v) where
  to_Hetcons_Message r1b = do
    {logDebugSH "to_Hetcons_Message Recursive_1b"
    ;hetcons_message_1a <- to_Hetcons_Message $ recursive_1b_proposal r1b -- this is a verified thing, so it will just pull the known signed version
    ;hetcons_message_2as' <- mapM to_Hetcons_Message $ toList $ recursive_1b_conflicting_phase2as r1b
    ;let signature_of_2a h = signed_Hash_signature $ signed_Indices_signature ((hetcons_Message_phase_2as h)!(fromIntegral $ hetcons_Message_index h))
    ;let hetcons_message_2as = sortOn signature_of_2a $ toList hetcons_message_2as'
    ;let hetcons_message = foldl fuse_Hetcons_Messages hetcons_message_1a hetcons_message_2as
    ;let signed_index_1a = (hetcons_Message_phase_1as hetcons_message_1a)!(fromIntegral $ hetcons_Message_index hetcons_message_1a)
    ;crypto_id <- get_my_crypto_id
    ;private_key <- get_my_private_key
    ;generator <- drgNew
    ;let type_descriptor = sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
    ;signature <- sign crypto_id private_key type_descriptor generator $ ByteString.concat $
      ((signed_Hash_signature $ signed_Index_signature signed_index_1a):
       (map signature_of_2a hetcons_message_2as))
    ;let hetcons_message_with_1b_on_end =  fuse_Hetcons_Messages hetcons_message (
      -- In this message, we're not going to ensure anything but the 1b actually references the right stuff,
      -- since all that will be replaced by its equivalents from hetcons_message anyway.
           default_Hetcons_Message {
             hetcons_Message_proposals = Vector.empty
            ,hetcons_Message_phase_1as = Vector.singleton signed_index_1a
            ,hetcons_Message_phase_1bs = Vector.singleton $ default_Phase_1b_Indices{
                phase_1b_Indices_index_1a = 0
               ,phase_1b_Indices_indices_2a = HashSet.fromList $ map fromIntegral [0..((length hetcons_message_2as) - 1)]
               ,phase_1b_Indices_signature = signature
              }
            ,hetcons_Message_phase_2as = Vector.fromList $ map (\h -> (hetcons_Message_phase_2as h)!(fromIntegral $ hetcons_Message_index h)) hetcons_message_2as
           })
    ; return hetcons_message_with_1b_on_end{hetcons_Message_index = fromIntegral $ (Vector.length $ hetcons_Message_phase_1bs hetcons_message_with_1b_on_end) - 1}
    }

instance {-# OVERLAPPING #-} (Hetcons_State s, Value v) => To_Hetcons_Message (Hetcons_Transaction s v) (Recursive_2a v) where
  to_Hetcons_Message (Recursive_2a r1bs) = do
    {logDebugSH "to_Hetcons_Message Recursive_2a"
    ;hetcons_message_1bs <- mapM to_Hetcons_Message $ toList r1bs
    ;let indices_1b h = (hetcons_Message_phase_1bs h)!(fromIntegral $ hetcons_Message_index h)
    ;crypto_id <- get_my_crypto_id
    ;private_key <- get_my_private_key
    ;generator <- drgNew
    ;let type_descriptor = sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR
    ;signature <- sign crypto_id private_key type_descriptor generator $ ByteString.concat $ sort $ map signature_bytestring $ toList r1bs
    -- using foldr ensures that the "old value" (which in this case is the newly constructed Signed_Indices) will always be on the right
    -- this ensures that the newly constructed Signed_Indices will be the last in the list of 2as
    ;let hetcons_message = foldr fuse_Hetcons_Messages (default_Hetcons_Message{
             hetcons_Message_phase_1bs = Vector.fromList $ map indices_1b hetcons_message_1bs
            ,hetcons_Message_phase_2as = Vector.singleton (default_Signed_Indices{signed_Indices_indices = HashSet.fromList $ map fromIntegral [0..((length hetcons_message_1bs) - 1)]
                                                                         ,signed_Indices_signature = signature})
           })
           hetcons_message_1bs
    ;return hetcons_message{hetcons_Message_index = fromIntegral $ (Vector.length (hetcons_Message_phase_2as hetcons_message)) - 1}
    }

instance {-# OVERLAPPING #-} (Hetcons_State s, Value v) => To_Hetcons_Message (Hetcons_Transaction s v) (Recursive_2b v) where
  to_Hetcons_Message (Recursive_2b r1bs) = logDebugSH "to_Hetons_Message Recursive_2b" >> to_Hetcons_Message (Recursive_2a r1bs)

instance {-# OVERLAPPING #-} (Hetcons_State s, Value v) => To_Hetcons_Message (Hetcons_Transaction s v) (Recursive_Proof_of_Consensus v) where
  -- must use foldr to ensure the "old element", which starts off as default_Hetcons_message, is on the right.
  to_Hetcons_Message (Recursive_Proof_of_Consensus r2bs) = logDebugSH "to_Hetcons_Message Recursive_Proof_of_Consensus" >>
                                                           (mapM to_Hetcons_Message $ toList r2bs) >>= (return . (foldr fuse_Hetcons_Messages default_Hetcons_Message))

-- | returns a Hetcons_Message which "fuses" two input ones.
--   The vector fields of the first input will be prefixes of the vector fields of the output.
--   The index will be the index of the first input.
--   Thus, it will decode exactly the same as the first input.
--   The non-duplicate elements of the second input will be appended to the vector fields of the first.
--   The indices of the appended elements will be adjusted for the new vectors.
fuse_Hetcons_Messages :: Hetcons_Message -> Hetcons_Message -> Hetcons_Message
fuse_Hetcons_Messages hetcons_message@Hetcons_Message
                      { hetcons_Message_proposals = proposals_1
                      , hetcons_Message_phase_1as = phase_1as_1
                      , hetcons_Message_phase_1bs = phase_1bs_1
                      , hetcons_Message_phase_2as = phase_2as_1}
                      Hetcons_Message
                      { hetcons_Message_proposals = proposals_2
                      , hetcons_Message_phase_1as = phase_1as_2
                      , hetcons_Message_phase_1bs = phase_1bs_2
                      , hetcons_Message_phase_2as = phase_2as_2} =
  let (proposals, proposals_2_lookup) = fuse_vectors id id proposals_1 proposals_2
      (phase_1as, phase_1as_2_lookup) = fuse_vectors (signed_Hash_signature . signed_Index_signature)
                                                     (\x -> x{signed_Index_index = fromIntegral $ proposals_2_lookup!(fromIntegral $ signed_Index_index x)}) 
                                                     phase_1as_1
                                                     phase_1as_2
      (phase_1bs, phase_1bs_2_lookup) = fuse_vectors (signed_Hash_signature . phase_1b_Indices_signature)
                                                     (\x -> x{phase_1b_Indices_index_1a = fromIntegral $ phase_1as_2_lookup!(fromIntegral $ phase_1b_Indices_index_1a x)
                                                             ,phase_1b_Indices_indices_2a = HashSet.map (fromIntegral . (phase_2as_2_lookup!) . fromIntegral) $ phase_1b_Indices_indices_2a x})
                                                     phase_1bs_1
                                                     phase_1bs_2
      (phase_2as, phase_2as_2_lookup) = fuse_vectors (signed_Hash_signature . signed_Indices_signature)
                                                     (\x -> x{signed_Indices_indices = HashSet.map (fromIntegral . (phase_1bs_2_lookup!) . fromIntegral) $ signed_Indices_indices x})
                                                     phase_2as_1
                                                     phase_2as_2
   in hetcons_message
      { hetcons_Message_proposals = proposals
      , hetcons_Message_phase_1as = phase_1as
      , hetcons_Message_phase_1bs = phase_1bs
      , hetcons_Message_phase_2as = phase_2as}
     


-- | a helper function for fuse_Hetcons_Messages
--   Inputs:
--   - representative: extracts a value from a vector element. Comparing these values is how we know two elements are "equivalent"
--   - proposals_2_transform: a function to be applied to all elements of the second vector that are appended to the first in the output
--   - proposals_1: The first vector in the fusion. It will be a prefix of the output vector
--   - proposals_2: the second vector in the fusion. Some of its elements will be appended to the first in the output vector,
--                  specifically those with no matching representative in the first vector.
--   Outputs:
--   - The fused vector
--   - A vector which matches proposals_2, but with each element replaced by its index in the fused vector
fuse_vectors :: (Eq a, Hashable a) => (b -> a) -> (b -> b) -> Vector b -> Vector b -> (Vector b, Vector Int)
fuse_vectors representative proposals_2_transform proposals_1 proposals_2 =
  let -- Trying to efficiently reverse indexing in a de-duplicated list of proposals (arbitrary type, called that for historical reasons)
      -- for this, we make a vector proposals_2_lookup, where proposals_2_lookup!i is the index of proposals_2!i in the new vector proposals:
      -- forall i . proposals!(proposals_2_lookup!i) = proposals_2!i
      -- along the way, we also make proposal_index_lookup, a reverse map of the vector proposals
      -- we do this by walking along proposals 2, construcing a backwards list (r_proposals_2) of all the proposals not yet seen (including in proposals_1)
      -- We also keep a matching reverse list of the index in proposals of all the elements of proposals_2
      -- we check these things by building the proposal_index_lookup
      (num_proposals, r_proposals_2, proposal_index_lookup, r_proposals_2_lookup) =
        foldr (\proposal (i, new_proposals, hashmap, r_prop2) -> (
                let r = representative proposal
                 in case HashMap.lookup r hashmap of
                      Just j  -> (i,                                    new_proposals,            hashmap, j:r_prop2)
                      Nothing -> (i+1, (proposals_2_transform proposal):new_proposals, insert r i hashmap, i:r_prop2)
              ))
              ( length proposals_1
               ,[]
               ,HashMap.fromList $ zip (map representative $ toList proposals_1) [0..]
               ,[]
              )
              proposals_2
      proposals = proposals_1 ++ (Vector.fromList $ reverse r_proposals_2)
      proposals_2_lookup = Vector.fromList $ reverse r_proposals_2_lookup
   in (proposals, proposals_2_lookup)