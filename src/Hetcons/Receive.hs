{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hetcons.Receive () where

import Hetcons.Conflicting_2as    (conflicting_2as)
import Hetcons.Contains_Value     (Contains_1bs, extract_1bs)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Hetcons_State      (Hetcons_State, Hetcons_State_Var, modify_and_read, default_Hetcons_State)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Receive_Message
  (Receive_Message
    ,run_Receive_Message_IO
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
import Hetcons.Signed_Message     (Verified, original, signed, sign, Recursive_1a, Recursive_1b, Recursive_2a, Recursive_2b, Recursive_Proof_of_Consensus, Parsable)

import Hetcons_Consts             (sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR)
import Hetcons_Types              (Crypto_ID
                                  ,default_Phase_1b
                                  ,phase_1b_proposal
                                  ,phase_1b_conflicting_phase2as
                                  ,Signed_Message
                                  )

import Control.Exception.Base     (throw)
import Control.Monad              (mapM, mapM_)
import Control.Monad.Except       (throwError, catchError, MonadError)
import Control.Monad.Reader       (MonadReader, Reader, runReader, reader, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.State        (StateT, runStateT, get, put,state)
import Crypto.Random              (drgNew)
import Data.ByteString.Lazy       (ByteString)
import Data.HashSet               (HashSet, insert, toList, fromList,  empty)
import Data.Serialize             (Serialize)

sign_m :: (Serialize a) => a -> Receive_Message Signed_Message
sign_m m = do
  { crypto_id <- get_my_crypto_id
  ; private_key <- get_my_private_key
  ; gen <- drgNew
  ; with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m}

instance Receivable (Verified Recursive_1a) where
  receive r1a = do
    { state <- get_state
    ; conflicting <- mapM sign_m $ toList $ conflicting_2as state r1a
    ; send default_Phase_1b {phase_1b_proposal = signed r1a
                            ,phase_1b_conflicting_phase2as = fromList conflicting}}

instance Receivable (Verified Recursive_1b) where
  receive r1b = return () -- TODO: write this for real

instance Receivable (Verified Recursive_2a) where
  receive r1b = return () -- TODO: write this for real

instance Receivable (Verified Recursive_2b) where
  receive r1b = return () -- TODO: write this for real

instance Receivable (Verified Recursive_Proof_of_Consensus) where
  receive r1b = return () -- TODO: write this for real
