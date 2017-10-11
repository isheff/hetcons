{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Defines what each type of server does upon receiving each type of message.
--   Thus, the consensus protocol is largely defined here.
--   Note that each message receipt will be in an atomic transaction.
module Hetcons.Receive () where

import Hetcons.Conflicting_2as ( conflicting_2as )
import Hetcons.Hetcons_Exception (Hetcons_Exception(Hetcons_Exception_Invalid_Proposal_1a))
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
     ,get_my_crypto_id)
import Hetcons.Send ()
import Hetcons.Signed_Message
    ( Encodable
     ,Parsable
     ,Recursive_2a(Recursive_2a)
     ,Recursive_1b(Recursive_1b)
        ,recursive_1b_non_recursive
        ,recursive_1b_proposal
        ,recursive_1b_conflicting_phase2as
     ,Verified
     ,Recursive_1a
     ,Recursive_2b(Recursive_2b)
     ,Recursive(non_recursive)
     ,Recursive_Proof_of_Consensus(Recursive_Proof_of_Consensus)
     ,Monad_Verify(verify)
     ,signed
     ,sign
     ,original )
import Hetcons.Value
    ( Contains_Value(extract_value)
     ,Contains_1a(extract_1a)
     ,Contains_1bs(extract_1bs)
     ,extract_observer_quorums
     ,extract_ballot
     ,Value
       ,valid
     ,conflicts
    )

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
     ,default_Phase_1b
     ,default_Invalid_Proposal_1a
     ,invalid_Proposal_1a_offending_proposal
     ,invalid_Proposal_1a_explanation)

import Control.Monad ( mapM, mapM_ )
import Control.Monad.Except ( MonadError(throwError) )
import Crypto.Random ( drgNew )
import Data.Foldable ( maximum )
import Data.Hashable (Hashable)
import Data.HashSet ( HashSet, toList, member, insert, fromList, size )
import qualified Data.HashSet as HashSet ( map, filter, empty )

-- | Helper function which signs a message using the Crypto_ID and Private_Key provided by the Mondic environment.
sign_m :: (Value v, Encodable a, Hetcons_State s) => a -> Hetcons_Transaction s v Signed_Message
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
instance (Value v, Eq v, Hashable v, Parsable (Hetcons_Transaction (Participant_State v) v v)) => Receivable (Participant_State v) v (Verified (Recursive_1a v)) where
  receive r1a = do
    { if valid r1a -- Checking validity here may seem odd, since we receive values inside other stuff, like 1bs.
         then return () -- However, the first time we receive a value, we always must end up here.
         else throwError $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
                             invalid_Proposal_1a_offending_proposal = non_recursive $ original r1a,
                             invalid_Proposal_1a_explanation = Just "This value is not itself considered valid."}
    ; let naive_1b = default_Phase_1b {phase_1b_proposal = signed r1a}
    ; state <- get_state
    -- TODO: non-pairwise conflicts
    ; let conflicting_ballots = HashSet.map extract_ballot $
             HashSet.filter (conflicts . fromList . (:[Recursive_1b {
                                                          recursive_1b_non_recursive = naive_1b
                                                         ,recursive_1b_proposal = r1a
                                                         ,recursive_1b_conflicting_phase2as = HashSet.empty}]) . original ) state
      -- If we've seen this 1a before, or we've seen one with a greater ballot that conflicts
    ; if ((not (null conflicting_ballots)) && ((extract_ballot r1a) <= (maximum conflicting_ballots)))
         then return ()
         else do { conflicting <- mapM sign_m $ toList $ conflicting_2as state r1a
                 ; send (naive_1b {phase_1b_conflicting_phase2as = fromList conflicting})}}

-- | Participant receives 1B
--   If we've received this 1B before, or one that conflicts but has a higher ballot number, do nothing.
--   Otherwise, we try to assemble a 2A out of all the 1Bs we've received for this ballot, and if we have enough (if that 2A is valid), we send it.
instance forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Participant_State v) v v)) => Receivable (Participant_State v) v (Verified (Recursive_1b v)) where
  receive r1b = do
    { old_state <- get_state
    -- TODO: non-pairwise conflicts
    ; let conflicting_ballots = HashSet.map extract_ballot $ HashSet.filter (conflicts . fromList . (:[r1b])) old_state
    ; if ((member r1b old_state) || -- If we've received this 1b before, or received something of greater ballot number (below)
         ((not (null conflicting_ballots)) &&
         ((extract_ballot r1b) < (maximum conflicting_ballots))))
         then return ()
         else do { my_crypto_id <- get_my_crypto_id
                 ; if (Just my_crypto_id) == (signed_Hash_crypto_id $ signed_Message_signature $ signed r1b) -- if this 1b is from me
                      then return ()
                      else receive ((extract_1a r1b) :: Verified (Recursive_1a v)) -- ensure we've received the 1a for this message before we store any 1bs
                 ; mapM_ receive ((extract_1bs $ original r1b) :: (HashSet (Verified (Recursive_1b v)))) -- receive all prior 1bs contained herein
                 ; state <- update_state (\s -> let new_state = insert r1b s in (new_state, new_state))
                 ; let potential_2a = Recursive_2a $
                         HashSet.filter ((((extract_1a r1b) :: Verified (Recursive_1a v)) ==) . extract_1a) $ -- all the 1bs with the same proposal
                         HashSet.filter ((((extract_value r1b) :: v) ==) . extract_value) state
                 ; case well_formed_2a potential_2a of
                     (Right _)-> do { signed <- sign_m $ ((non_recursive potential_2a) :: Phase_2a)
                                    ; (v :: (Verified (Recursive_2a v))) <- verify signed
                                    ; send v}
                     (Left _) -> return ()
                 ; send r1b}} -- echo the 1b

-- | Participant receives 2A
--   Upon receiving a 2A, send a corresponding 2B.
--   Note that the only way for a participant to receive a 2A is for that participant to itself send it.
--   It can't come in over the wire.
instance (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Participant_State v) v v)) => Receivable (Participant_State v) v (Verified (Recursive_2a v)) where
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
instance forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v)) => Receivable (Observer_State v) v (Verified (Recursive_2b v)) where
  receive r2b = do
    { old_state <- get_state
    ; if (member r2b old_state)
         then return () -- Else, we make a Proof_of_Consensus using what we've received, and see if that's valid.
         else do { let state = insert r2b old_state
                 ; put_state state
                 ; let potential_proof' =
                         HashSet.filter ((((extract_1a r2b) :: Verified (Recursive_1a v)) ==) . extract_1a) $ -- all the 2bs with the same proposal
                         HashSet.filter ((((extract_value r2b) :: v) ==) . extract_value) state  -- all the 2bs with the same value
                 -- filter for only the longest 2bs from each sender
                 ; let potential_proof = HashSet.filter(\v2b->let same_crypto_id = HashSet.filter (((signed_Hash_crypto_id $ signed_Message_signature $ signed v2b) ==) .
                                                                                                     signed_Hash_crypto_id . signed_Message_signature . signed)
                                                                                                  potential_proof'
                                                               in all (\x -> let (Recursive_2b y) = original x
                                                                                 (Recursive_2b r) = original v2b
                                                                              in (size y) <= (size r))
                                                                      same_crypto_id)
                                                       potential_proof'
                 ; if (length (observers_proven potential_proof)) > 0
                      then do { signed <- sign_m (default_Proof_of_Consensus { proof_of_Consensus_phase_2bs = HashSet.map signed potential_proof})
                              ; (v :: (Verified (Recursive_Proof_of_Consensus v))) <- verify signed
                              ; send v}
                      else return ()
                 ; send r2b}}

-- | Observer receives Proof_of_Consensus
--   Note that this can only be received if this Observer sends it to itself.
--   It cannot come in over the wire.
--   TODO: what do we do here? We have consensus (at least for some observers).
instance (Value v) => Receivable (Observer_State v) v (Verified (Recursive_Proof_of_Consensus v)) where
  receive rpoc = return ()
