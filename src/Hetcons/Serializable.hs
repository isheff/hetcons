-- | Implement Serialization for the Thrift messages using Thrift's Encode and Decode functions.
-- | This is mostly used for testing, but also necessary for some derived types for some reason.
module Hetcons.Serializable () where

import Charlotte_Types
    ( Signed_Message
       ,encode_Signed_Message
       ,decode_Signed_Message
     ,Proposal_1a
       ,encode_Proposal_1a
       ,decode_Proposal_1a
     ,Proof_of_Consensus
       ,encode_Proof_of_Consensus
       ,decode_Proof_of_Consensus
     ,Phase_2b
       ,encode_Phase_2b
       ,decode_Phase_2b
     ,Phase_2a
       ,encode_Phase_2a
       ,decode_Phase_2a
     ,Phase_1b
       ,encode_Phase_1b
       ,decode_Phase_1b )

import Control.Monad ( liftM, mapM_ )
import Data.ByteString.Lazy ( unpack )
import Data.Serialize ( Serialize, get, put )
import Data.Serialize.Get ( remaining, getLazyByteString )
import Data.Serialize.Put ( putWord8 )
import Thrift.Protocol.Compact ( CompactProtocol(CompactProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Proposal_1a where
  put = (mapM_ putWord8) . unpack . (encode_Proposal_1a (CompactProtocol EmptyTransport))
  get = liftM (decode_Proposal_1a (CompactProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Phase_1b where
  put = (mapM_ putWord8) . unpack . (encode_Phase_1b (CompactProtocol EmptyTransport))
  get = liftM (decode_Phase_1b (CompactProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Phase_2a where
  put = (mapM_ putWord8) . unpack . (encode_Phase_2a (CompactProtocol EmptyTransport))
  get = liftM (decode_Phase_2a (CompactProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Phase_2b where
  put = (mapM_ putWord8) . unpack . (encode_Phase_2b (CompactProtocol EmptyTransport))
  get = liftM (decode_Phase_2b (CompactProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})

-- | Serialize and deserialize this Thrift type using Thrift's functions.
-- | This should probably be done natively by Thrift.
instance Serialize Proof_of_Consensus where
  put = (mapM_ putWord8) . unpack . (encode_Proof_of_Consensus (CompactProtocol EmptyTransport))
  get = liftM (decode_Proof_of_Consensus (CompactProtocol EmptyTransport)) (
         do { length <- remaining
            ; getLazyByteString (fromIntegral length)})
