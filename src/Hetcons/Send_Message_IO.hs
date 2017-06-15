{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Hetcons.Send_Message_IO (send_Message_IO) where

import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Signed_Message     (Verified, original, Recursive_1b, Parsable, sign)

import Hetcons_Types              (Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus, Signed_Message)

import Control.Monad              (mapM_)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.HashSet               (HashSet, insert, toList, empty)

-- TODO: Implement this for real
send_Message_IO _ = putStrLn "UNIMPLEMENTED: send_Message_IO\n"

