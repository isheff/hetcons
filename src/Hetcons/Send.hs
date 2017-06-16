{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import Hetcons_Consts         (sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR)

import Crypto.Random          (drgNew)
import Data.Serialize         (Serialize)

instance {-# OVERLAPPABLE #-} (Serialize a, Recursive a b, Parsable b, Add_Sent (Verified b), Receivable (Verified b)) => Sendable a where
  send m = do { crypto_id <- get_my_crypto_id
              ; private_key <- get_my_private_key
              ; gen <- drgNew
              ; signed <- with_errors $ sign crypto_id private_key sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen m
              ; (verified :: Verified b) <- with_errors $ verify signed
              ; receive verified
              ; add_sent verified}
