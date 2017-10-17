{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The properties of a 1A message,such as how to parse it, extract valuese from it, etc.
module Hetcons.Instances_1a () where

import Hetcons.Quorums (Monad_Verify_Quorums, verify_quorums )
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,Recursive
       ,non_recursive
     ,Recursive_1a(Recursive_1a)
       ,recursive_1a_non_recursive
       ,recursive_1a_filled_in
       ,recursive_1a_value
     ,Parsable
       ,parse
     ,Verified )
import Hetcons.Value
    ( Contains_Value(extract_value)
     ,Contains_1a(extract_1a)
     ,Value
    )

import Charlotte_Types
    ( Proposal_1a(proposal_1a_observers, proposal_1a_value)
      ,encode_Proposal_1a )

import Data.Hashable ( Hashable, hashWithSalt )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-- | Hash a Recursive_1a by hashing its non-recursive version
instance (Value v) => Hashable (Recursive_1a v) where
  hashWithSalt s x = hashWithSalt s ((non_recursive x) :: Proposal_1a)

-- | The non-recursive version of a Recursive_1a is a Proposal_1a
instance (Value v) => Recursive Proposal_1a (Recursive_1a v) where
  non_recursive = recursive_1a_non_recursive

-- | Encode a Proposal_1a to a bytestring using Thrift
instance {-# OVERLAPPING #-} Encodable Proposal_1a where
  encode = encode_Proposal_1a (BinaryProtocol EmptyTransport)

-- | To parse a Recursive_1a object, we verify observer graph, and fill in quorums
instance {-# OVERLAPPING #-} (Value v, Parsable (m v), Monad_Verify_Quorums m) => Parsable (m (Recursive_1a v)) where
  parse payload =
    do { non_recursive <- parse payload
       ; value <- parse $ proposal_1a_value non_recursive
       ; filled_in <- verify_quorums non_recursive
       ; return Recursive_1a {
              recursive_1a_non_recursive = non_recursive
             ,recursive_1a_value = value
             ,recursive_1a_filled_in = non_recursive {proposal_1a_observers = Just filled_in}}}

-- | A 1A contains a 1A, itself.
instance {-# OVERLAPPING #-} (Value v) => Contains_1a (Verified (Recursive_1a v)) v where
  extract_1a = id

-- | a Recursive_1a contains a value
instance {-# OVERLAPPING #-} (Value v) => Contains_Value (Recursive_1a v) v where
  extract_value = recursive_1a_value



