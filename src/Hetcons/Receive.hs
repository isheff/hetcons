{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hetcons.Receive () where

import Hetcons.Conflicting_2as    (conflicting_2as)
import Hetcons.Contains_Value     (Contains_1bs, extract_1bs, extract_1a, extract_value, extract_ballot, extract_observer_quorums)
import Hetcons.Hetcons_Exception  (Hetcons_Exception(Hetcons_Exception_Invalid_Proposal_1a))
import Hetcons.Hetcons_State      (Hetcons_State, Participant_State, Observer_State, Participant_State_Var, modify_and_read, default_State)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Receive_Message
  (Hetcons_Transaction
    ,run_Hetcons_Transaction_IO
    ,get_state
    ,put_state
    ,update_state
    ,get_my_crypto_id
    ,get_my_private_key
    ,with_errors
  ,Add_Sent
    ,add_sent
  ,Receivable
    ,receive
  ,Sendable
    ,send)
import Hetcons.Send               ()
import Hetcons.Send_Message_IO    (send_Message_IO)
import Hetcons.Signed_Message     (Verified, original, signed, sign, verify, non_recursive, Recursive_1a, Recursive_1b, Recursive_2a, Recursive_2b, Recursive_Proof_of_Consensus, Parsable)

import Hetcons_Consts             (sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR)
import Hetcons_Types              (Crypto_ID
                                  ,default_Phase_1b
                                  ,phase_1b_proposal
                                  ,phase_1b_conflicting_phase2as
                                  ,Signed_Message
                                  ,default_Phase_2a
                                  ,Phase_2a(Phase_2a)
                                    ,phase_2a_phase_1bs
                                  ,default_Phase_2b
                                  ,phase_2b_phase_1bs
                                  ,default_Proof_of_Consensus
                                  ,proof_of_Consensus_phase_2bs
                                  ,signed_Hash_crypto_id
                                  ,signed_Message_signature
                                  ,default_Invalid_Proposal_1a
                                  )

import Control.Exception.Base     (throw)
import Control.Monad              (mapM, mapM_)
import Control.Monad.Except       (throwError, catchError, MonadError)
import Control.Monad.Reader       (MonadReader, Reader, runReader, reader, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.State        (StateT, runStateT, get, put,state)
import Crypto.Random              (drgNew)
import Data.ByteString.Lazy       (ByteString)
import Data.Foldable              (maximum)
import Data.HashSet               (HashSet, insert, toList, fromList,  empty, member)
import qualified Data.HashSet as HashSet (map, filter)
import Data.Serialize             (Serialize)

sign_m :: (Serialize a, Hetcons_State s) => a -> Hetcons_Transaction s Signed_Message
sign_m m = do
  { crypto_id <- get_my_crypto_id
  ; private_key <- get_my_private_key
  ; gen <- drgNew
  ; with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m}


--  How Participants Receive ---------------------------------------------------

instance Receivable Participant_State (Verified Recursive_1a) where
  receive r1a = do
    { state <- get_state
    ; let ballots_with_matching_quorums = HashSet.map extract_ballot $ HashSet.filter (((extract_observer_quorums r1a) ==) . extract_observer_quorums) state
      -- If we've seen this 1a before, or we've seen one with a greater ballot and the same quorums
    ; if ((not (null ballots_with_matching_quorums)) && ((extract_ballot r1a) <= (maximum ballots_with_matching_quorums)))
         then return ()
         else do { conflicting <- mapM sign_m $ toList $ conflicting_2as state r1a
                 ; send (default_Phase_1b {phase_1b_proposal = signed r1a
                                          ,phase_1b_conflicting_phase2as = fromList conflicting})}}

instance Receivable Participant_State (Verified Recursive_1b) where
  receive r1b = do
    { old_state <- get_state
    ; let ballots_with_matching_quorums = HashSet.map extract_ballot $ HashSet.filter (((extract_observer_quorums r1b) ==) . extract_observer_quorums) old_state
    ; if ((member r1b old_state) || -- If we've received this 1b before, or received something of greater ballot number (below)
         ((not (null ballots_with_matching_quorums)) &&
         ((extract_ballot r1b) < (maximum ballots_with_matching_quorums))))
         then return ()
         else do { my_crypto_id <- get_my_crypto_id
                 ; if (Just my_crypto_id) == (signed_Hash_crypto_id $ signed_Message_signature $ signed r1b) -- if this 1b is from me
                      then return ()
                      else receive $ extract_1a r1b -- ensure we've received the 1a for this message before we store any 1bs
                 ; mapM_ receive $ extract_1bs $ original r1b -- receive all prior 1bs contained herein
                 ; let state = insert r1b old_state
                 ; put_state state -- update the state to include this 1b
                 ; signed <- sign_m (default_Phase_2a { phase_2a_phase_1bs = HashSet.map signed $
                     HashSet.filter (((extract_1a r1b) ==) . extract_1a) $ -- all the 1bs with the same proposal
                     HashSet.filter (((extract_value r1b) ==) . extract_value) state})  -- all the 1bs with the same value
                 ; case ((verify signed) :: (Either Hetcons_Exception (Verified Recursive_2a))) of
                     Left e -> return ()
                     Right v -> send v
                 ; send r1b}} -- I'm assuming that sending a 2a will send all the 1bs in it.


instance Receivable Participant_State (Verified Recursive_2a) where
  -- | Recall that there is no actual way to receive a 2a other than sending it to yourself.
  -- | Therefore, we can be assured that this 2a comes to use exactly once, and that all 1bs therein have been received.
  receive r2a = send $ default_Phase_2b {phase_2b_phase_1bs = phase_2a_phase_1bs $ non_recursive $ original r2a}



-- How Observers Receive ------------------------------------------------------
instance Receivable Observer_State (Verified Recursive_2b) where
  receive r2b = do
    { old_state <- get_state
    ; if (member r2b old_state)
         then return () -- Else, we make a Proof_of_Consensus using what we've received, and see if that's valid.
         else do { let state = insert r2b old_state
                 ; put_state state
                 ; signed <- sign_m (default_Proof_of_Consensus { proof_of_Consensus_phase_2bs = HashSet.map signed $
                     HashSet.filter (((extract_1a r2b) ==) . extract_1a) $ -- all the 2bs with the same proposal
                     HashSet.filter (((extract_value r2b) ==) . extract_value) state})  -- all the 2bs with the same value
                 ; case ((verify signed) :: (Either Hetcons_Exception (Verified Recursive_Proof_of_Consensus))) of
                     Left e -> return () -- this proof isn't valid, and shouldn't be sent out (maybe not enough 2bs yet) However, we still have to echo the 2b
                     Right v -> send v
                 ; send r2b}}

instance Receivable Observer_State (Verified Recursive_Proof_of_Consensus) where
  receive rpoc = return () -- TODO: what do we do here? We have consensus (at least for some observers).
