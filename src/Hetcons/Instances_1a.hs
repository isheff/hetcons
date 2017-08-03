{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Instances_1a () where

import Hetcons.Contains_Value
    ( Contains_Value(extract_value)
     ,Contains_1a(extract_1a) )
import Hetcons.Quorums ( verify_quorums )
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,Recursive
       ,non_recursive
     ,Recursive_1a(Recursive_1a)
       ,recursive_1a_non_recursive
       ,recursive_1a_filled_in
     ,Parsable
       ,parse
     ,Verified )

import Hetcons_Types
    ( Proposal_1a(proposal_1a_observers, proposal_1a_value)
      ,encode_Proposal_1a )

import Data.Hashable ( Hashable, hashWithSalt )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

instance Hashable Recursive_1a where
  hashWithSalt s x = hashWithSalt s ((non_recursive x) :: Proposal_1a)

instance Recursive Proposal_1a Recursive_1a where
  non_recursive = recursive_1a_non_recursive


instance {-# OVERLAPPING #-} Encodable Proposal_1a where
  encode = encode_Proposal_1a (BinaryProtocol EmptyTransport)

-- | For a 1a object, we verify observer graph, and fill in quorums
instance {-# OVERLAPPING #-} Parsable Recursive_1a where
  parse payload =
    do { non_recursive <- parse payload
       ; filled_in <- verify_quorums non_recursive
       ; return Recursive_1a {
              recursive_1a_non_recursive = non_recursive
       ,recursive_1a_filled_in = non_recursive {proposal_1a_observers = Just filled_in}}}

instance {-# OVERLAPPING #-} Contains_1a (Verified Recursive_1a) where
  extract_1a = id
instance {-# OVERLAPPING #-} Contains_Value Proposal_1a where
  extract_value = proposal_1a_value
instance {-# OVERLAPPING #-} Contains_Value Recursive_1a where
  extract_value = extract_value . recursive_1a_filled_in
