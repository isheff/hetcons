{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The properties of a 1A message,such as how to parse it, extract valuese from it, etc.
module Hetcons.Instances_1a () where

import Hetcons.Quorums (Monad_Verify_Quorums, verify_quorums )
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,To_Hetcons_Message
       ,to_Hetcons_Message
     ,From_Hetcons_Message
       ,from_Hetcons_Message
     ,Recursive_1a(Recursive_1a)
       ,recursive_1a_non_recursive
       ,recursive_1a_filled_in
       ,recursive_1a_value
     ,Parsable
       ,parse
     ,Verified
       ,original)
import Hetcons.Value
    ( Contains_Value(extract_value)
     ,Contains_1a(extract_1a)
     ,Value
    )

import Charlotte_Types
    ( Proposal_1a(proposal_1a_observers, proposal_1a_value)
      ,encode_Proposal_1a
     ,Hetcons_Message(Hetcons_Message)
      ,hetcons_Message_proposals
      ,hetcons_Message_phase_1as
      ,default_Hetcons_Message
     ,signed_Index_index
    )

import Crypto.Random (drgNew)
import Data.Hashable ( Hashable, hashWithSalt )
import Data.Vector ((!))
import Thrift.Protocol.Compact ( CompactProtocol(CompactProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-- | Hash a Recursive_1a by hashing its non-recursive version
instance (Value v) => Hashable (Recursive_1a v) where
  hashWithSalt s x = hashWithSalt s ((recursive_1a_non_recursive x) :: Proposal_1a)


-- | Encode a Proposal_1a to a bytestring using Thrift
instance {-# OVERLAPPING #-} Encodable Proposal_1a where
  encode = encode_Proposal_1a (CompactProtocol EmptyTransport)

-- | To parse a Recursive_1a object, we verify observer graph, and fill in quorums
instance {-# OVERLAPPING #-} (Value v, Parsable (m v), Monad_Verify_Quorums m) => Parsable (m (Recursive_1a v)) where
  parse payload = parse payload >>= from_non_recursive

-- | A 1A contains a 1A, itself.
instance {-# OVERLAPPING #-} (Value v) => Contains_1a (Verified (Recursive_1a v)) v where
  extract_1a = id

-- | a Recursive_1a contains a value
instance {-# OVERLAPPING #-} (Value v) => Contains_Value (Recursive_1a v) v where
  extract_value = recursive_1a_value

-- | To get a phase_1a from a Hetcons_Message, we choose the first phase_1a in its list, and then select and parse the proposal it references.
instance {-# OVERLAPPING #-} (Value v, Parsable (m v), Monad_Verify_Quorums m) => From_Hetcons_Message (m (Recursive_1a v)) where
  from_Hetcons_Message v =
    from_non_recursive ((hetcons_Message_proposals $ original v)!(fromIntegral $ signed_Index_index ((hetcons_Message_phase_1as $ original v)!0)))



-- | A helper function to construct a Recursive_1a from a non-recursive 1a
from_non_recursive :: (Value v, Parsable (m v), Monad_Verify_Quorums m) => Proposal_1a -> (m (Recursive_1a v)) 
from_non_recursive non_recursive = do
  { value <- parse $ proposal_1a_value non_recursive
  ; filled_in <- verify_quorums non_recursive
  ; return Recursive_1a {
         recursive_1a_non_recursive = non_recursive
        ,recursive_1a_value = value
        ,recursive_1a_filled_in = non_recursive {proposal_1a_observers = Just filled_in}}}
