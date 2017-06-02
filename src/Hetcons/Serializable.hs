{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Serializable () where


import Hetcons_Types (Proposal_1a
                        ,proposal_1a_observers
                        ,encode_Proposal_1a
                        ,decode_Proposal_1a
                     ,Phase_1b
                        ,phase_1b_proposal
                        ,phase_1b_conflicting_phase2as
                        ,encode_Phase_1b
                        ,decode_Phase_1b
                     ,Phase_2a
                        ,phase_2a_phase_1bs
                        ,default_Phase_2a
                        ,encode_Phase_2a
                        ,decode_Phase_2a
                     ,Phase_2b
                        ,phase_2b_phase_1bs
                        ,default_Phase_2b
                        ,encode_Phase_2b
                        ,decode_Phase_2b
                     ,Proof_of_Consensus
                        ,proof_of_Consensus_phase_2bs
                        ,default_Proof_of_Consensus
                        ,encode_Proof_of_Consensus
                        ,decode_Proof_of_Consensus
                     )

import           Control.Monad          (liftM, mapM_)
import           Data.ByteString.Lazy   (unpack)
import           Data.Foldable          (length)
import           Data.Serialize         (Serialize
                                        ,get
                                        ,put)
import           Data.Serialize.Get     (remaining, getLazyByteString)
import           Data.Serialize.Put     (putWord8)
import           Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import           Thrift.Transport.Empty (EmptyTransport(EmptyTransport))

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Proposal_1a where
  put = (mapM_ putWord8) . unpack . (encode_Proposal_1a (BinaryProtocol EmptyTransport))
  get = liftM (decode_Proposal_1a (BinaryProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Phase_1b where
  put = (mapM_ putWord8) . unpack . (encode_Phase_1b (BinaryProtocol EmptyTransport))
  get = liftM (decode_Phase_1b (BinaryProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Phase_2a where
  put = (mapM_ putWord8) . unpack . (encode_Phase_2a (BinaryProtocol EmptyTransport))
  get = liftM (decode_Phase_2a (BinaryProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Phase_2b where
  put = (mapM_ putWord8) . unpack . (encode_Phase_2b (BinaryProtocol EmptyTransport))
  get = liftM (decode_Phase_2b (BinaryProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Proof_of_Consensus where
  put = (mapM_ putWord8) . unpack . (encode_Proof_of_Consensus (BinaryProtocol EmptyTransport))
  get = liftM (decode_Proof_of_Consensus (BinaryProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})
