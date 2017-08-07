{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | How each type of message is sent (within a Hetcons Transaction).
--   In the simplest case, we simply `add_sent`, which merely adds the message to the set of messages to be sent at the completion of the transaction.
--   However, for some message types, we expect to `receive` the message sent within the same transaction.
--   In particular, when we `receive` a 1A, we `send` a corresponding 1B, and expect it to be added to the state all within one transaction.
module Hetcons.Send () where

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
      Verified,
      Recursive,
      sign )
import Hetcons.Hetcons_State
    ( Hetcons_State, Participant_State, Observer_State )

import Hetcons_Consts ( sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR )
import Hetcons_Types
    ( Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus )

import Crypto.Random ( drgNew )

-- | A utility function to sign a message using the `Crypto_ID` and private key from the monad, and produce a `Verified` version.
sign_and_verify :: (Monad_Verify b (Hetcons_Transaction s), Encodable a, Parsable b, Hetcons_State s, Recursive a b) => a -> Hetcons_Transaction s (Verified b)
sign_and_verify m = do { crypto_id <- get_my_crypto_id
                       ; private_key <- get_my_private_key
                       ; gen <- drgNew
                       ; signed <-  sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m
                       ; verify signed}

-- | In general, when possible, send a `Verified` message by first receiving it yourself, and then adding it to the messages to be sent at the end of the transaction.
--   Note that sending a message will inherently involve receiving it BEFORE the transaction is finished.
--   Infinite loops of messages would be bad.
instance {-# OVERLAPPABLE #-} (Hetcons_State s, Receivable s a, Add_Sent a) => Sendable s a where
  send m = do { receive m
              ; add_sent m}

--------------------------------------------------------------------------------
--                                Participants                                --
--------------------------------------------------------------------------------

-- | Participants can receive 1as, so this send will run a receive within the same transaction.
--   Also, they shouldn't be sending 1as, but whatever...
instance {-# OVERLAPPING #-} (Receivable Participant_State (Verified (Recursive_1a))) => Sendable Participant_State Proposal_1a where
  send m = do { (verified :: Verified (Recursive_1a)) <- sign_and_verify m
              ; send verified}

-- | Participants can receive 1bs, so this send will run a receive within the same transaction.
instance {-# OVERLAPPING #-} (Receivable Participant_State (Verified (Recursive_1b))) => Sendable Participant_State Phase_1b where
  send m = do { (verified :: Verified (Recursive_1b)) <- sign_and_verify m
              ; send verified}

-- | Participants can receive 2as, so this send will run a receive within the same transaction.
instance {-# OVERLAPPING #-} (Receivable Participant_State (Verified (Recursive_2a))) => Sendable Participant_State Phase_2a where
  send m = do { (verified :: Verified (Recursive_2a)) <- sign_and_verify m
              ; send verified}

-- | Participants can't receive 2bs, so this send will not run a receive
instance {-# OVERLAPPING #-} Sendable Participant_State Phase_2b where
  send m = do { (verified :: Verified (Recursive_2b)) <- sign_and_verify m
              ; add_sent verified}

-- | Participants can't receive Proof_of_Consensus, so this send will not run a receive
--   Also, Participants really should never send a proof of consensus...
instance {-# OVERLAPPING #-} Sendable Participant_State Proof_of_Consensus where
  send m = do { (verified :: Verified (Recursive_Proof_of_Consensus)) <- sign_and_verify m
              ; add_sent verified}

--------------------------------------------------------------------------------
--                                 Observers                                  --
--------------------------------------------------------------------------------

-- | Observers can't receive 1as, so this send won't run a receive
--   Also, they shouldn't be sending 1as, but whatever...
instance {-# OVERLAPPING #-} Sendable Observer_State Proposal_1a where
  send m = do { (verified :: Verified (Recursive_1a)) <- sign_and_verify m
              ; add_sent verified}

-- | Observers can't receive 1bs, so this send won't run a receive
--   Also, they shouldn't be sending 1bs, but whatever...
instance {-# OVERLAPPING #-} Sendable Observer_State Phase_1b where
  send m = do { (verified :: Verified (Recursive_1b)) <- sign_and_verify m
              ; add_sent verified}

-- | Observers can't receive 2as, so this send won't run a receive
--   Also, they shouldn't be sending 2as, but whatever...
instance {-# OVERLAPPING #-} Sendable Observer_State Phase_2a where
  send m = do { (verified :: Verified (Recursive_2a)) <- sign_and_verify m
              ; add_sent verified}

-- | Observers can receive 2bs, so this send will run a receive
instance {-# OVERLAPPING #-} (Receivable Observer_State (Verified (Recursive_2b))) => Sendable Observer_State Phase_2b where
  send m = do { (verified :: Verified (Recursive_2b)) <- sign_and_verify m
              ; send verified}

-- | Observers can receive Proof_of_Consensus, so this send will run a receive
instance {-# OVERLAPPING #-} (Receivable Observer_State (Verified (Recursive_Proof_of_Consensus))) => Sendable Observer_State Proof_of_Consensus where
  send m = do { (verified :: Verified (Recursive_Proof_of_Consensus)) <- sign_and_verify m
              ; send verified}
