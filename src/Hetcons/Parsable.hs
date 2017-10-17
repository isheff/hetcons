{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Hetcons.Parsable
    (Parsable
      ,parse
    ) where

import Charlotte_Types
    (Proposal_1a
       ,decode_Proposal_1a
     ,Public_Crypto_Key(Public_Crypto_Key
                       ,public_Crypto_Key_public_crypto_key_x509)
     ,Proof_of_Consensus
       ,decode_Proof_of_Consensus
     ,Phase_2b
       ,decode_Phase_2b
     ,Phase_2a
       ,decode_Phase_2a
     ,Phase_1b
       ,decode_Phase_1b
     ,Slot_Value
       ,decode_Slot_Value
    )

import Data.ByteString.Lazy ( ByteString )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-- | We have messages serialized for transport within Signed_Messages.
--   However, deserializing them into their thrift data structures is not enough,
--    if we want to recursively parse and verify the signed messags they carry within themselves.
--   Therefore, we create the Parsable class for stuff which might require such recursive verification.
class Parsable a where
  -- | The parse function is meant to deserialize an object, but also deserialize and verify any signed messages within it.
  --   Of course, this depends on the type of the object.
  parse :: ByteString -> a

-- | Parse a Slot_Value using Thrift
instance {-# OVERLAPPING #-} (Monad m) => Parsable (m Slot_Value) where
  parse = return . (decode_Slot_Value (BinaryProtocol EmptyTransport))

-- | Parse a Proposal_1a using Thrift
instance {-# OVERLAPPING #-} (Monad m) => Parsable (m Proposal_1a) where
  parse = return . (decode_Proposal_1a (BinaryProtocol EmptyTransport))

-- | Parse a Phase_1b using Thrift
instance {-# OVERLAPPING #-} (Monad m) => Parsable (m Phase_1b) where
  parse = return . (decode_Phase_1b (BinaryProtocol EmptyTransport))

-- | Parse a Phase_2a using Thrift
instance {-# OVERLAPPING #-} (Monad m) => Parsable (m Phase_2a) where
  parse = return . (decode_Phase_2a (BinaryProtocol EmptyTransport))

-- | Parse a Phase_2b using Thrift
instance {-# OVERLAPPING #-} (Monad m) => Parsable (m Phase_2b) where
  parse = return . (decode_Phase_2b (BinaryProtocol EmptyTransport))

-- | Parse a Proof_of_Consensus using Thrift
instance {-# OVERLAPPING #-} (Monad m) => Parsable (m Proof_of_Consensus) where
  parse = return . (decode_Proof_of_Consensus (BinaryProtocol EmptyTransport))


