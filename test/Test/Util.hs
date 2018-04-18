{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Util () where

import Hetcons.Hetcons_Exception ( Hetcons_Exception(Hetcons_Exception_Unparsable_Hashable_Message) )
import Hetcons.Quorums ( Monad_Verify_Quorums
                           ,verify_quorums'
                           ,verify_quorums)
import Hetcons.Signed_Message (Encodable
                                ,encode
                              ,From_Hetcons_Message
                                ,from_Hetcons_Message
                              ,verify_hetcons_message
                              ,Parsable
                                ,parse
                                ,verify'
                                ,verify
                              ,Recursive_1a
                              ,Recursive_1b
                              ,Recursive_2a
                              ,Recursive_2b
                              ,Recursive_Proof_of_Consensus
                              ,Monad_Verify)

import Charlotte_Types (Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus, Hetcons_Message
                     ,default_Unparsable_Hashable_Message
                     ,unparsable_Hashable_Message_explanation
                     ,unparsable_Hashable_Message_message )

import Control.Monad.Except ( MonadError, throwError )
import Data.Serialize ( Serialize, encodeLazy, decodeLazy )
import Data.Text.Lazy (pack)

-- this instance is used in testing only
instance {-# OVERLAPPABLE #-} (Monad_Verify_Quorums m
                              ,MonadError Hetcons_Exception m)
                               => Monad_Verify Hetcons_Message m where
  verify = verify_hetcons_message
instance {-# OVERLAPPABLE #-} (Monad_Verify_Quorums m
                              ,MonadError Hetcons_Exception m, From_Hetcons_Message (m a))
                               => Monad_Verify a m where
  verify = verify'



-- TODO: this instance is used in testing only, so we should move it over to tests
instance {-# OVERLAPPABLE #-} (MonadError Hetcons_Exception m) => Monad_Verify_Quorums m where
  verify_quorums = verify_quorums'

instance {-# OVERLAPPABLE #-} (Serialize a) => Encodable a where
  encode = encodeLazy


-- | By default, anythign serializable is simply deserialized.
-- | the only possible error is if parsing fails
instance {-# OVERLAPPABLE #-} (Monad m, MonadError Hetcons_Exception m, Serialize a) => Parsable (m a) where
  parse payload =
    case decodeLazy payload of
      (Left e) ->
        throwError $ Hetcons_Exception_Unparsable_Hashable_Message default_Unparsable_Hashable_Message {
           unparsable_Hashable_Message_message = payload
          ,unparsable_Hashable_Message_explanation = Just $ pack $ "I was unable to parse this message payload:\n" ++ e}
      (Right x) -> return x
