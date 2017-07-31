{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Instances_1b_2a (well_formed_2a) where

import Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
     ,Contains_1a
        ,extract_1a
        ,extract_observer_quorums
     ,Ballot
        ,extract_ballot
     ,Contains_1bs
        ,extract_1bs
    )
import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_Invalid_Phase_2a
                       ,Hetcons_Exception_Invalid_Phase_1b) )
import Hetcons.Instances_1a ()
import Hetcons.Signed_Message
    ( Recursive_1b(Recursive_1b)
       ,recursive_1b_non_recursive
       ,recursive_1b_proposal
       ,recursive_1b_conflicting_phase2as
     ,Verified
     ,Parsable
       ,parse
     ,Recursive
       ,non_recursive
     ,Recursive_2a (Recursive_2a )
     ,Monad_Verify(verify)
     ,signed
     ,original )
import Hetcons.Value ( conflicts )

import Hetcons_Types
    ( Participant_ID(participant_ID_crypto_id)
     ,Invalid_Phase_2a(invalid_Phase_2a_explanation
                      ,invalid_Phase_2a_offending_phase_2a)
     ,Invalid_Phase_1b(invalid_Phase_1b_explanation
                      ,invalid_Phase_1b_offending_phase_1b)
     ,Phase_2a(phase_2a_phase_1bs)
     ,Signed_Message(signed_Message_signature)
     ,Phase_1b(phase_1b_conflicting_phase2as, phase_1b_proposal)
     ,Signed_Hash(signed_Hash_crypto_id)
     ,default_Phase_2a
     ,default_Invalid_Phase_2a
     ,default_Invalid_Phase_1b )
import Control.Monad ( mapM_ )
import Control.Monad.Except ( MonadError(throwError) )
import Data.Foldable ( null, length, maximumBy )
import Data.Hashable ( Hashable, hashWithSalt )
import Data.HashMap.Strict ( elems )
import Data.HashSet
    ( unions, toList, intersection, insert, fromList )
import qualified Data.HashSet as HashSet ( map )
import Data.List ( head )
import Data.Maybe ( catMaybes )
import Data.Text.Lazy ( pack )
import Data.Traversable ( mapM )





-- | Phase_1b s carry 1a and 2a messages with them.
-- | Recursive_1b s carry parsed and verified versions of these.
instance Hashable Recursive_1b where
  hashWithSalt s x = hashWithSalt s ((non_recursive x) :: Phase_1b)

instance Recursive Phase_1b Recursive_1b where
  non_recursive = recursive_1b_non_recursive


-- | Phase_2a s carry phase 1b messages with them.
-- | Recursive_2a s carry parsed and verified versions of these.
instance Hashable Recursive_2a where
  hashWithSalt s (Recursive_2a x) = hashWithSalt s x
instance Recursive Phase_2a Recursive_2a where
  non_recursive (Recursive_2a x) = default_Phase_2a {phase_2a_phase_1bs = HashSet.map signed x}



well_formed_1b :: (MonadError Hetcons_Exception m) => Recursive_1b -> m ()
well_formed_1b (Recursive_1b {
                  recursive_1b_non_recursive = non_recursive
                 ,recursive_1b_proposal = proposal
                 ,recursive_1b_conflicting_phase2as = conflicting_phase2as})
  = mapM_ (\x -> if (extract_observer_quorums proposal) /= (extract_observer_quorums x)
                then throwError $ Hetcons_Exception_Invalid_Phase_1b (default_Invalid_Phase_1b {
                       invalid_Phase_1b_offending_phase_1b = non_recursive
                       ,invalid_Phase_1b_explanation = Just $ pack "not all contained phase_2as had the same quorums as this phase_1b"
                       })
                else if not $ conflicts $ fromList [extract_value proposal, extract_value x]
                        then throwError $ Hetcons_Exception_Invalid_Phase_1b (default_Invalid_Phase_1b {
                               invalid_Phase_1b_offending_phase_1b = non_recursive
                               ,invalid_Phase_1b_explanation = Just $ pack "not all contained phase_2as conflict with the proposal"
                               })
                        else return ())
      $ toList conflicting_phase2as



-- | For a 1b object, we verify the proposal and 2a messages it carries, and parse the original message
-- | We're also going to verify the 1b's well-formedness, because that has to happen somewhere.
instance {-# OVERLAPPING #-} Parsable Recursive_1b where
  parse payload =
    do { non_recursive <- parse payload -- (Either Hetcons_Exception) Monad
       ; proposal <- verify $ phase_1b_proposal non_recursive
       ; conflicting_phase2as <- mapM verify $ toList $ phase_1b_conflicting_phase2as non_recursive
       ; let r1b = Recursive_1b {
                      recursive_1b_non_recursive = non_recursive
                     ,recursive_1b_proposal = proposal
                     ,recursive_1b_conflicting_phase2as = fromList conflicting_phase2as}
       ; well_formed_1b r1b
       ; return r1b}

well_formed_2a :: (MonadError Hetcons_Exception m) => Recursive_2a -> m ()
well_formed_2a r2a@(Recursive_2a s) =
  do { if 1 /= (length $ HashSet.map extract_value s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "there were 1bs with different values in this 2a, or no 1bs at all"})
          else return ()
     ; if 1 /= (length $ HashSet.map extract_observer_quorums s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2a (default_Invalid_Phase_2a{
                         invalid_Phase_2a_offending_phase_2a = non_recursive r2a
                        ,invalid_Phase_2a_explanation = Just $ pack "there were 1bs with different observers in this 2a"})
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

-- | for a 2a message, we parse the original mesage, and verify the 1b messages it carries.
instance {-# OVERLAPPING #-} Parsable Recursive_2a where
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






instance {-# OVERLAPPING #-} Contains_1a Recursive_1b where
  extract_1a = extract_1a . recursive_1b_proposal


-- | The "value" carried by a 1b is actually tricky: it may be set by the 2a s carried within.
-- | This relies on having already checked that the phase_2as do indeed conflict with the given 1b
instance {-# OVERLAPPING #-} Contains_Value Recursive_1b where
  extract_value (Recursive_1b {
                   recursive_1b_conflicting_phase2as = phase_2as
                  ,recursive_1b_proposal = proposal})
    = if null phase_2as
         then extract_value proposal
         else extract_value $ maximumBy (\x y -> compare (extract_ballot x) (extract_ballot y)) phase_2as


instance {-# OVERLAPPING #-} Contains_1bs (Recursive_1b) where
  extract_1bs (Recursive_1b {recursive_1b_conflicting_phase2as = phase_2as}) = unions $ map extract_1bs $ toList phase_2as

instance {-# OVERLAPPING #-} Contains_1bs (Verified (Recursive_1b)) where
  extract_1bs b = insert b $ extract_1bs b



instance {-# OVERLAPPING #-} Contains_1a Recursive_2a where
  extract_1a (Recursive_2a x) = extract_1a $ head $ toList x

-- | The "value" carried by a 2a is actually tricky:
-- | This relies on this 2a already having been verified to ensure that, for instance, all 1bs within have the same value
instance {-# OVERLAPPING #-} Contains_Value Recursive_2a where
  extract_value (Recursive_2a x) = extract_value $ head $ toList x

instance {-# OVERLAPPING #-} Contains_1bs (Recursive_2a) where
  extract_1bs (Recursive_2a x) = x
