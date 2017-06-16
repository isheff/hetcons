{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Send () where

import Hetcons.Receive_Message
  (Receive_Message
    ,get_my_crypto_id
    ,get_my_private_key
    ,with_errors
  ,Add_Sent
    ,add_sent
  ,Receivable
    ,receive
  ,Sendable
    ,send)
import Hetcons.Signed_Message (Verified, Recursive, Parsable, verify, sign)
import Hetcons.Signed_Message     (Verified, original, signed, sign, Recursive_1a, Recursive_1b, Recursive_2a, Recursive_2b, Recursive_Proof_of_Consensus, Parsable)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()

import Hetcons_Consts         (sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR)
import Hetcons_Types          (Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus)

import Crypto.Random          (drgNew)
import Data.Serialize         (Serialize)

-- | Note that sending a message will inherently involve receiving int BEFORE the transaction is finished.
-- | Infinite loops of messages would be bad.
instance {-# OVERLAPPABLE #-} (Receivable (Verified (Recursive_1a))) => Sendable Proposal_1a where
  send m = do { crypto_id <- get_my_crypto_id
              ; private_key <- get_my_private_key
              ; gen <- drgNew
              ; signed <- with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m
              ; (verified :: Verified (Recursive_1a)) <- with_errors $ verify signed
              ; receive verified
              ; add_sent verified}

instance {-# OVERLAPPABLE #-} (Receivable (Verified (Recursive_1b))) => Sendable Phase_1b where
  send m = do { crypto_id <- get_my_crypto_id
              ; private_key <- get_my_private_key
              ; gen <- drgNew
              ; signed <- with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m
              ; (verified :: Verified (Recursive_1b)) <- with_errors $ verify signed
              ; receive verified
              ; add_sent verified}

instance {-# OVERLAPPABLE #-} (Receivable (Verified (Recursive_2a))) => Sendable Phase_2a where
  send m = do { crypto_id <- get_my_crypto_id
              ; private_key <- get_my_private_key
              ; gen <- drgNew
              ; signed <- with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m
              ; (verified :: Verified (Recursive_2a)) <- with_errors $ verify signed
              ; receive verified
              ; add_sent verified}

instance {-# OVERLAPPABLE #-} (Receivable (Verified (Recursive_2b))) => Sendable Phase_2b where
  send m = do { crypto_id <- get_my_crypto_id
              ; private_key <- get_my_private_key
              ; gen <- drgNew
              ; signed <- with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m
              ; (verified :: Verified (Recursive_2b)) <- with_errors $ verify signed
              ; receive verified
              ; add_sent verified}

instance {-# OVERLAPPABLE #-} (Receivable (Verified (Recursive_Proof_of_Consensus))) => Sendable Proof_of_Consensus where
  send m = do { crypto_id <- get_my_crypto_id
              ; private_key <- get_my_private_key
              ; gen <- drgNew
              ; signed <- with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m
              ; (verified :: Verified (Recursive_Proof_of_Consensus)) <- with_errors $ verify signed
              ; receive verified
              ; add_sent verified}
