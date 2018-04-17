{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | How each type of message is sent (within a Hetcons Transaction).
--   In the simplest case, we simply `add_sent`, which merely adds the message to the set of messages to be sent at the completion of the transaction.
--   However, for some message types, we expect to `receive` the message sent within the same transaction.
--   In particular, when we `receive` a 1A, we `send` a corresponding 1B, and expect it to be added to the state all within one transaction.
module Hetcons.Send (
      Send_1a,
        send_1a,
      Send_1b,
        send_1b,
      Send_2a,
        send_2a,
      Send_2b,
        send_2b,
      Send_Proof_of_Consensus,
        send_proof_of_consensus
  ) where

import Hetcons.Instances_of_To_Hetcons_Message ()
import Hetcons.Receive_Message
    ( Hetcons_Transaction,
      get_my_crypto_id,
      get_my_private_key,
      Add_Sent,
      add_sent,
      Receivable,
      receive,
      Sendable,
      send )
import Hetcons.Signed_Message
    ( Parsable,
      Encodable,
      Monad_Verify(verify),
      Recursive_1a,
      Recursive_1b,
      Recursive_2a,
      Recursive_2b,
      Recursive_Proof_of_Consensus,
      To_Hetcons_Message,
        to_Hetcons_Message,
      Verified,
      sign )
import Hetcons.Hetcons_State
    ( Hetcons_State, Participant_State, Observer_State )
import Hetcons.Value (Value)

import Charlotte_Consts ( sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR )
import Charlotte_Types
    (Hetcons_Message, Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus )

import Crypto.Random ( drgNew )
import Data.Hashable (Hashable)

-- | A utility function to sign a message using the `Crypto_ID` and private key from the monad, and produce a `Verified` version.
sign_and_verify :: (Value v, Monad_Verify b (Hetcons_Transaction s v), To_Hetcons_Message (Hetcons_Transaction s v) a,  Hetcons_State s) =>
                   a -> Hetcons_Transaction s v (Verified b)
sign_and_verify message = (to_Hetcons_Message message) >>= verify

-- | In general, when possible, send a `Verified` message by first receiving it yourself, and then adding it to the messages to be sent at the end of the transaction.
--   Note that sending a message will inherently involve receiving it BEFORE the transaction is finished.
--   Infinite loops of messages would be bad.
instance {-# OVERLAPPABLE #-} (Hetcons_State s, Receivable s v a, Add_Sent a v) => Sendable s v a where
  send m = do { receive m
              ; add_sent m}

--------------------------------------------------------------------------------
--                                Participants                                --
--------------------------------------------------------------------------------

-- | Participants can receive 1as, so this send will run a receive within the same transaction.
--   Also, they shouldn't be sending 1as, but whatever...
class Send_1a s v where
  send_1a :: Hetcons_Message -> Hetcons_Transaction s v ()

instance {-# OVERLAPPING #-} forall v . (Value v, Parsable (Hetcons_Transaction (Participant_State v) v v), Receivable (Participant_State v) v (Verified (Recursive_1a v))) =>
         Send_1a (Participant_State v) v where
  send_1a m = do { (verified :: Verified (Recursive_1a v)) <- sign_and_verify m
                 ; send verified}

-- | Participants can receive 1bs, so this send will run a receive within the same transaction.
class Send_1b s v where
  send_1b :: Hetcons_Message -> Hetcons_Transaction s v ()
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Participant_State v) v v),
                                    Receivable (Participant_State v) v (Verified (Recursive_1b v))) =>
                             Send_1b (Participant_State v) v where
  send_1b m = do { (verified :: Verified (Recursive_1b v)) <- sign_and_verify m
                 ; send verified}

-- | Participants can receive 2as, so this send will run a receive within the same transaction.
class Send_2a s v where
  send_2a :: Hetcons_Message -> Hetcons_Transaction s v ()
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Participant_State v) v v),
                                    Receivable (Participant_State v) v (Verified (Recursive_2a v))) =>
                             Send_2a (Participant_State v) v where
  send_2a m = do { (verified :: Verified (Recursive_2a v)) <- sign_and_verify m
                 ; send verified}

-- | Participants can't receive 2bs, so this send will not run a receive
class Send_2b s v where
  send_2b :: Hetcons_Message -> Hetcons_Transaction s v ()
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Participant_State v) v v)) => Send_2b (Participant_State v) v where
  send_2b m = do { (verified :: Verified (Recursive_2b v)) <- sign_and_verify m
                 ; add_sent verified}

-- | Participants can't receive Proof_of_Consensus, so this send will not run a receive
--   Also, Participants really should never send a proof of consensus...
class Send_Proof_of_Consensus s v where
  send_proof_of_consensus :: Hetcons_Message -> Hetcons_Transaction s v ()
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Participant_State v) v v)) =>
                             Send_Proof_of_Consensus (Participant_State v) v where
  send_proof_of_consensus m = do { (verified :: Verified (Recursive_Proof_of_Consensus v)) <- sign_and_verify m
                                 ; add_sent verified}

--------------------------------------------------------------------------------
--                                 Observers                                  --
--------------------------------------------------------------------------------

-- | Observers can't receive 1as, so this send won't run a receive
--   Also, they shouldn't be sending 1as, but whatever...
instance {-# OVERLAPPING #-} forall v . (Value v, Parsable (Hetcons_Transaction (Observer_State v) v v)) => Send_1a (Observer_State v) v where
  send_1a m = do { (verified :: Verified (Recursive_1a v)) <- sign_and_verify m
                 ; add_sent verified}

-- | Observers can't receive 1bs, so this send won't run a receive
--   Also, they shouldn't be sending 1bs, but whatever...
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v)) => Send_1b (Observer_State v) v where
  send_1b m = do { (verified :: Verified (Recursive_1b v)) <- sign_and_verify m
                 ; add_sent verified}

-- | Observers can't receive 2as, so this send won't run a receive
--   Also, they shouldn't be sending 2as, but whatever...
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v)) => Send_2a (Observer_State v) v where
  send_2a m = do { (verified :: Verified (Recursive_2a v)) <- sign_and_verify m
                 ; add_sent verified}

-- | Observers can receive 2bs, so this send will run a receive
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v),
                                    Receivable (Observer_State v) v (Verified (Recursive_2b v))) =>
                             Send_2b (Observer_State v) v where
  send_2b m = do { (verified :: Verified (Recursive_2b v)) <- sign_and_verify m
                 ; send verified}

-- | Observers can receive Proof_of_Consensus, so this send will run a receive
instance {-# OVERLAPPING #-} forall v . (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v),
                                    Receivable (Observer_State v) v (Verified (Recursive_Proof_of_Consensus v))) =>
                             Send_Proof_of_Consensus (Observer_State v) v where
  send_proof_of_consensus m = do { (verified :: Verified (Recursive_Proof_of_Consensus v)) <- sign_and_verify m
                                 ; send verified}
