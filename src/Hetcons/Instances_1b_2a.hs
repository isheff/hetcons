{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Defines the properties of 1B and 2A messages, most notably which typeclasses they're instances of
--   1B and 2A share a submodule because they're so inter-dependent
module Hetcons.Instances_1b_2a (well_formed_2a) where

import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_Invalid_Phase_2a
                       ,Hetcons_Exception_Invalid_Phase_1b) )
import Hetcons.Instances_1a ()
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,From_Hetcons_Message
       ,from_Hetcons_Message
     ,Recursive_1a
     ,Recursive_1b(Recursive_1b)
       ,recursive_1b_proposal
       ,recursive_1b_conflicting_phase2as
     ,Verified
     ,Parsable
       ,parse
     ,Recursive_2a (Recursive_2a )
     ,Monad_Verify(verify)
     ,signed
     ,original )
import Hetcons.Value
    ( Contains_Value
        ,extract_value
     ,Contains_1a
        ,extract_1a
        ,extract_observer_quorums
     ,Contains_Quorums
     ,Ballot
     ,Contains_Ballot
        ,extract_ballot
     ,Contains_1bs
        ,extract_1bs
     ,Value
     ,conflicts
    )

import Charlotte_Types
    ( Participant_ID(participant_ID_crypto_id)
     ,Invalid_Phase_2a(invalid_Phase_2a_explanation
                      ,invalid_Phase_2a_offending_phase_2a)
     ,Invalid_Phase_1b(invalid_Phase_1b_explanation
                      ,invalid_Phase_1b_offending_phase_1b)
     ,Phase_2a(phase_2a_phase_1bs)
              ,encode_Phase_2a
     ,Hetcons_Message(Hetcons_Message)
       ,hetcons_Message_proposals
       ,hetcons_Message_phase_1as
       ,hetcons_Message_phase_1bs
       ,hetcons_Message_phase_2as
     ,Phase_1b_Indices(Phase_1b_Indices)
       ,phase_1b_Indices_index_1a
       ,phase_1b_Indices_indices_2b
     ,Phase_1b(phase_1b_conflicting_phase2as, phase_1b_proposal)
              ,encode_Phase_1b
              ,default_Phase_1b
     ,Signed_Hash(signed_Hash_crypto_id)
     ,default_Phase_2a
     ,default_Invalid_Phase_2a
     ,default_Invalid_Phase_1b )
import Control.Monad ( mapM_ )
import Control.Monad.Except ( MonadError(throwError) )
import Data.Foldable ( null, length, maximumBy, toList )
import Data.Hashable ( Hashable, hashWithSalt )
import Data.HashMap.Strict ( elems )
import Data.HashSet
    ( HashSet, unions, intersection, insert, fromList )
import qualified Data.HashSet as HashSet ( map )
import Data.List ( head )
import Data.Maybe ( catMaybes )
import Data.Text.Lazy ( pack )
import Data.Traversable ( mapM, forM )
import Data.Vector (imap, (!), singleton)
import qualified Data.Vector as Vector (map, empty)
import Thrift.Protocol.Compact ( CompactProtocol(CompactProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-------------------------------------------------------------------------------
--                                Phase 1B                                   --
-------------------------------------------------------------------------------

-- | Phase_1b s can be encoded to ByteStrings using Thrift
instance {-# OVERLAPPING #-} Encodable Phase_1b where
  encode = encode_Phase_1b (CompactProtocol EmptyTransport)


-- | Recursive_1b s can be hashed by hashing their non-recursive version
instance (Value v) => Hashable (Recursive_1b v) where
  hashWithSalt s x = hashWithSalt s (recursive_1b_proposal x, recursive_1b_conflicting_phase2as x)

-- | All the 1Bs contained within the (2As within the) Recursive_1b
instance {-# OVERLAPPING #-} (Value v) => Contains_1bs (Recursive_1b v) v where
  extract_1bs (Recursive_1b {recursive_1b_conflicting_phase2as = phase_2as}) = unions $ map extract_1bs $ toList phase_2as

-- | The 1Bs contained in a Verified Recursive_1b are slightly different, in that they include the Verified Recursive_1b itself.
instance {-# OVERLAPPING #-} (Value v) => Contains_1bs (Verified (Recursive_1b v)) v where
  extract_1bs b = insert b $ extract_1bs b

-- | 1Bs contain a 1A
instance {-# OVERLAPPING #-} (Value v) => Contains_1a (Recursive_1b v) v where
  extract_1a = extract_1a . recursive_1b_proposal

instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Ballot (Recursive_1b v) where
  extract_ballot x = extract_ballot ((extract_1a x) :: (Verified (Recursive_1a v)))

instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Quorums (Recursive_1b v) where
  extract_observer_quorums x = extract_observer_quorums ((extract_1a x) :: (Verified (Recursive_1a v)))

-- | The "value" carried by a 1b is actually tricky: it may be set by the 2a s carried within.
--   This relies on having already checked that the phase_2as do indeed conflict with the given 1b
--   If there are no 2As, we return the value of the contained 1A.
--   Otherwise, we return the value of the maximum 2A by Ballot.
instance {-# OVERLAPPING #-} (Value v) => Contains_Value (Recursive_1b v) v where
  extract_value (Recursive_1b {
                   recursive_1b_conflicting_phase2as = phase_2as
                  ,recursive_1b_proposal = proposal})
    = if null phase_2as
         then extract_value proposal
         else extract_value $ maximumBy (\x y -> compare (extract_ballot x) (extract_ballot y)) phase_2as

-- | Throws a Hetcons_Exception of the 1B is not well formed.
--   A 1b is "well formed" if all the 2A s it contains conflict with this 1B.
well_formed_1b :: (MonadError Hetcons_Exception m, Value v) => (Recursive_1b v) -> m ()
well_formed_1b (Recursive_1b {
                  recursive_1b_proposal = proposal
                 ,recursive_1b_conflicting_phase2as = conflicting_phase2as})
  = mapM_ (\x -> if not $ conflicts $ fromList [proposal, extract_1a x]
                then throwError $ Hetcons_Exception_Invalid_Phase_1b (default_Invalid_Phase_1b {
                       invalid_Phase_1b_offending_phase_1b = default_Phase_1b -- non_recursive
                       -- TODO: Hetcons_Message does not provide an offending Phase_1b to return. Fix that.
                       ,invalid_Phase_1b_explanation = Just $ pack "not all contained phase_2as conflict with the proposal. Hetcons_Message format does not provide an offending Phase_1b to return"
                       })
                else return ())
      $ toList conflicting_phase2as


-- | Construct a 1B from a Hetcons Message
--   Specifically, this will represent the first 1B in the list of 1Bs in the Hetcons_Message
instance {-# OVERLAPPING #-} (Value v, Monad_Verify (Recursive_1a v) m, Monad_Verify (Recursive_2a v) m) => From_Hetcons_Message (m (Recursive_1b v)) where
  from_Hetcons_Message verified_hetcons_message = do
    {let hetcons_message@Hetcons_Message
           {hetcons_Message_proposals = proposals
           ,hetcons_Message_phase_1as = phase_1as
           ,hetcons_Message_phase_1bs = phase_1bs
           ,hetcons_Message_phase_2as = phase_2as
           } = original verified_hetcons_message
    ;let phase_1b_indices@Phase_1b_Indices
           {phase_1b_Indices_index_1a   = index_1a
           ,phase_1b_Indices_indices_2b = indices_2b
           } = phase_1bs!0
    ;proposal <- verify Hetcons_Message{hetcons_Message_proposals = proposals
                                       ,hetcons_Message_phase_1as = singleton (phase_1as!(fromIntegral index_1a))
                                       ,hetcons_Message_phase_1bs = Vector.empty
                                       ,hetcons_Message_phase_2as = Vector.empty}
    ;conflicting_2as <- forM (toList indices_2b) (\k -> (do
      -- For each conflicing 2A, we must verify that 2a, however, we identify which 2A to decode by whichever is first in the list.
      -- Therefore, we swap the first element of the 2A list with the desired 2A vefore calling from_Hetcons_Message
      {let i = fromIntegral k
      ;let new_2as = imap (\j v -> if (j == 0) then phase_2as!i else (if (j == i) then phase_2as!0 else v)) phase_2as -- swap element 0 and i in phase_2as
      -- Since we've swapped about the 2As, we must also swap about all references to the 2As (which can be found in the 1Bs)
      ;let new_1bs = Vector.map (\phase_1b -> phase_1b{phase_1b_Indices_indices_2b = HashSet.map (\j -> if (j == 0) then k else (if (j == k) then 0 else j))
                                                                                                 $ phase_1b_Indices_indices_2b phase_1b})
                                phase_1bs
      ;verify hetcons_message{ -- TODO: this needlessly re-verifies the signatures, which are the same (we've just re-ordered the lists) let's not do that.
         hetcons_Message_phase_1bs = new_1bs
        ,hetcons_Message_phase_2as = new_2as
        }
      }))
    ;let answer = Recursive_1b {
        recursive_1b_proposal = proposal
       ,recursive_1b_conflicting_phase2as = fromList conflicting_2as
       }
    ;well_formed_1b answer
    ;return answer
    }


-------------------------------------------------------------------------------
--                                Phase 2A                                   --
-------------------------------------------------------------------------------

-- | Phase_2a s can be encoded to ByteStrings using Thrift
instance {-# OVERLAPPING #-} Encodable Phase_2a where
  encode = encode_Phase_2a (CompactProtocol EmptyTransport)


-- | Hash a Recursive_2a by hashing its non-recursive version
instance (Value v) => Hashable (Recursive_2a v) where
  hashWithSalt s (Recursive_2a x) = hashWithSalt s x

-- | A 2A contains 1Bs
instance {-# OVERLAPPING #-} (Value v) => Contains_1bs (Recursive_2a v) v where
  extract_1bs (Recursive_2a x) = x

-- | the 1A of a 2A message is the latest 1A (ballot number) present in all of its 1Bs
instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_1a (Recursive_2a v) v where
  extract_1a = (maximumBy (\x y -> compare (extract_ballot x) (extract_ballot y))) . (HashSet.map (extract_1a::(Verified(Recursive_1b v))->(Verified(Recursive_1a v)))) . extract_1bs

-- | A well-formed 2A has all contained 1Bs feature the same value.
--   Therefore, its value is the value of any one of its 1Bs
instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Value (Recursive_2a v) v where
  extract_value = (extract_value :: (Verified (Recursive_1b v)) -> v) . head . toList . (extract_1bs :: (Recursive_2a v) -> (HashSet (Verified (Recursive_1b v))))

instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Ballot (Recursive_2a v) where
  extract_ballot x = extract_ballot ((extract_1a x) :: (Verified (Recursive_1a v)))

instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Quorums (Recursive_2a v) where
  extract_observer_quorums x = extract_observer_quorums ((extract_1a x) :: (Verified (Recursive_1a v)))

-- | Throws a Hetcons_Exception of the 2A is not well formed.
--   A 2A is well-formed if all of the following hold:
--
--    * It contains some 1Bs
--
--    * All contained 1Bs have the same value
--
--    * All contained 1Bs have their Observer fields filled-in (we don't support not doing that)
--
--    * All contained 1Bs have the same Observers
--
--    * The contained 1Bs satisfy a quorum of Participants, as defined by at least one of the Observers
well_formed_2a :: forall m v . (MonadError Hetcons_Exception m, Value v, Hashable v, Eq v) => (Recursive_2a v) -> m ()
well_formed_2a r2a@(Recursive_2a s) =
  do { if 1 /= (length $ HashSet.map (extract_value :: (Verified (Recursive_1b v)) -> v) s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "there were 1bs with different values in this 2a, or no 1bs at all"})
          else return ()
          ; if 1 /= (length $ HashSet.map (extract_1a :: (Verified (Recursive_1b v)) -> (Verified (Recursive_1a v))) s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "there were 1bs with different 1as in this 2a"})
          else return ()
     ; let observers = extract_observer_quorums r2a
     ; if 0 == length observers
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "at this time, we require that observer quorums be listed by participant ID"})
          else return ()
     ; let quorums_crypto_ids = HashSet.map (HashSet.map participant_ID_crypto_id) $ unions $ elems observers
     ; let crypto_ids_of_1bs = fromList $ catMaybes $ toList $ HashSet.map (signed_Hash_crypto_id . signed_Message_signature . signed) s
     ; if all (\q -> (q /= (intersection q crypto_ids_of_1bs))) quorums_crypto_ids
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "this set of 1bs does not satisfy any known quorum"})
          else return ()
     }

-- | Parse a Recursive_2a (part of verifying it)
--   for a 2a message, we parse the original mesage, and verify the 1b messages it carries.
--   Also, we check its well-formed-ness
instance {-# OVERLAPPING #-} (Hashable v, Eq v, Value v, Monad_Verify (Recursive_1a v) m, Monad_Verify (Recursive_1b v) m) => Parsable (m (Recursive_2a v)) where
  parse payload =
    do { non_recursive <- parse payload
       ; l_set <- mapM verify $ toList $ phase_2a_phase_1bs non_recursive
       ; let set = fromList l_set
       ; if (length (HashSet.map (recursive_1b_proposal . original) set)) > 1
            then throwError $ Hetcons_Exception_Invalid_Phase_2a default_Invalid_Phase_2a {
                                 invalid_Phase_2a_offending_phase_2a = non_recursive
                                ,invalid_Phase_2a_explanation = Just $ pack "More than 1 proposal value present."}
            else return ()
       ; well_formed_2a $ Recursive_2a set
       ; return $ Recursive_2a set}
