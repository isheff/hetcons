{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Defines what each type of server does upon receiving each type of message.
--   Thus, the consensus protocol is largely defined here.
--   Note that each message receipt will be in an atomic transaction.
module Hetcons.Receive () where

import Hetcons.Conflicting_2as ( conflicting_2as )
import Hetcons.Contains_Value
    ( Contains_Value(extract_value)
     ,Contains_1a(extract_1a)
     ,Contains_1bs(extract_1bs)
     ,extract_observer_quorums
     ,extract_ballot )
import Hetcons.Hetcons_State
    ( Participant_State, Observer_State, Hetcons_State )
import Hetcons.Instances_1b_2a ( well_formed_2a )
import Hetcons.Instances_Proof_of_Consensus ( observers_proven )
import Hetcons.Receive_Message
    ( Sendable(send)
     ,Receivable
       ,receive
     ,Hetcons_Transaction
     ,update_state
     ,put_state
     ,get_state
     ,get_my_private_key
     ,get_my_crypto_id )
import Hetcons.Send ()
import Hetcons.Signed_Message
    ( Encodable
     ,Recursive_2a(Recursive_2a)
     ,Recursive_1b
     ,Verified
     ,Recursive_1a
     ,Recursive_2b
     ,Recursive(non_recursive)
     ,Recursive_Proof_of_Consensus
     ,Monad_Verify(verify)
     ,signed
     ,sign
     ,original )

import Hetcons_Consts ( sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR )
import Hetcons_Types
    ( Signed_Message(signed_Message_signature)
     ,Phase_2a(phase_2a_phase_1bs)
     ,Signed_Hash(signed_Hash_crypto_id)
     ,Phase_1b(phase_1b_conflicting_phase2as, phase_1b_proposal)
     ,Phase_2b(phase_2b_phase_1bs)
     ,Proof_of_Consensus(proof_of_Consensus_phase_2bs)
     ,default_Proof_of_Consensus
     ,default_Phase_2b
     ,default_Phase_1b )

import Control.Monad ( mapM, mapM_ )
import Crypto.Random ( drgNew )
import Data.Foldable ( maximum )
import Data.HashSet ( toList, member, insert, fromList )
import qualified Data.HashSet as HashSet ( map, filter )

-- | Helper function which signs a message using the Crypto_ID and Private_Key provided by the Mondic environment.
sign_m :: (Encodable a, Hetcons_State s) => a -> Hetcons_Transaction s Signed_Message
sign_m m = do
  { crypto_id <- get_my_crypto_id
  ; private_key <- get_my_private_key
  ; gen <- drgNew
  ; sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m}


--------------------------------------------------------------------------------
--                                Participants                                --
--------------------------------------------------------------------------------

-- | Participant receives 1A
--   If we've seen anything with this Ballot number or higher before (featuring the same Quorums), then do nothing.
--   Otherwise, send a 1B.
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

-- | Participant receives 1B
--   If we've received this 1B before, or one with matching quorums but a higher ballot number, do nothing.
--   Otherwise, we try to assemble a 2A out of all the 1Bs we've received for this ballot, and if we have enough (if that 2A is valid), we send it.
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
                 ; state <- update_state (\s -> let new_state = insert r1b s in (new_state, new_state))
                 ; let potential_2a = Recursive_2a $
                         HashSet.filter (((extract_1a r1b) ==) . extract_1a) $ -- all the 1bs with the same proposal
                         HashSet.filter (((extract_value r1b) ==) . extract_value) state
                 ; case well_formed_2a potential_2a of
                     (Right _)-> do { signed <- sign_m $ ((non_recursive potential_2a) :: Phase_2a)
                                    ; (v :: (Verified Recursive_2a)) <- verify signed
                                    ; send v}
                     (Left _) -> return ()
                 ; send r1b}} -- echo the 1b

-- | Participant receives 2A
--   Upon receiving a 2A, send a corresponding 2B.
--   Note that the only way for a participant to receive a 2A is for that participant to itself send it.
--   It can't come in over the wire.
instance Receivable Participant_State (Verified Recursive_2a) where
  -- | Recall that there is no actual way to receive a 2a other than sending it to yourself.
  --   Therefore, we can be assured that this 2a comes to us exactly once, and that all 1bs therein have been received.
  receive r2a = send $ default_Phase_2b {phase_2b_phase_1bs = phase_2a_phase_1bs $ non_recursive $ original r2a}

--------------------------------------------------------------------------------
--                                 Observers                                  --
--------------------------------------------------------------------------------

-- | Observer receives 2B
--   If we've received this 2b before, do nothing.
--   Otherwise, assemble all received 2Bs with the same proposal and value, and see if those form a valid Proof_of_Consensus
--   If they do, send that Proof_of_Consensus
instance Receivable Observer_State (Verified Recursive_2b) where
  receive r2b = do
    { old_state <- get_state
    ; if (member r2b old_state)
         then return () -- Else, we make a Proof_of_Consensus using what we've received, and see if that's valid.
         else do { let state = insert r2b old_state
                 ; put_state state
                 ; let potential_proof =
                         HashSet.filter (((extract_1a r2b) ==) . extract_1a) $ -- all the 2bs with the same proposal
                         HashSet.filter (((extract_value r2b) ==) . extract_value) state  -- all the 2bs with the same value
                 ; if (length (observers_proven potential_proof)) > 0
                      then do { signed <- sign_m (default_Proof_of_Consensus { proof_of_Consensus_phase_2bs = HashSet.map signed potential_proof})
                              ; (v :: (Verified Recursive_Proof_of_Consensus)) <- verify signed
                              ; send v}
                      else return ()
                 ; send r2b}}

-- | Observer receives Proof_of_Consensus
--   Note that this can only be received if this Observer sends it to itself.
--   It cannot come in over the wire.
--   TODO: what do we do here? We have consensus (at least for some observers).
instance Receivable Observer_State (Verified Recursive_Proof_of_Consensus) where
  receive rpoc = return ()
