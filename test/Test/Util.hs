{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Util () where

import Hetcons.Hetcons_Exception ( Hetcons_Exception )
import Hetcons.Quorums ( Monad_Verify_Quorums
                           ,verify_quorums'
                           ,verify_quorums)
import Hetcons.Signed_Message (Encodable
                                ,encode
                              ,Parsable
                                ,verify'
                                ,verify
                              ,Recursive_1a
                              ,Recursive_1b
                              ,Recursive_2a
                              ,Recursive_2b
                              ,Recursive_Proof_of_Consensus
                              ,Monad_Verify)

import Hetcons_Types (Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus)

import Control.Monad.Except ( MonadError )
import Data.Serialize ( Serialize, encodeLazy )

-- TODO: this instance is used in testing only, so we should move it over to tests
instance {-# OVERLAPPABLE #-} (Parsable Recursive_1a
                              ,Parsable Recursive_1b
                              ,Parsable Recursive_2a
                              ,Parsable Recursive_2b
                              ,Parsable Recursive_Proof_of_Consensus
                              ,Monad_Verify_Quorums m
                              ,MonadError Hetcons_Exception m, Parsable a)
                               => Monad_Verify a m where
  verify = verify'



-- TODO: this instance is used in testing only, so we should move it over to tests
instance {-# OVERLAPPABLE #-} (MonadError Hetcons_Exception m) => Monad_Verify_Quorums m where
  verify_quorums = verify_quorums'

instance {-# OVERLAPPABLE #-} (Serialize a) => Encodable a where
  encode = encodeLazy
