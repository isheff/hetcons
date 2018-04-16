{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines the properties of 2B messages, most notably which typeclasses they're instances of
module Hetcons.Instances_2b () where

import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_Invalid_Phase_2b) )
import Hetcons.Instances_1b_2a ()
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,From_Hetcons_Message
       ,from_Hetcons_Message
     ,Recursive_1a
     ,Recursive_1b(recursive_1b_proposal)
     ,Parsable
       ,parse
     ,Recursive_2b (Recursive_2b)
     ,Recursive_2a(Recursive_2a)
     ,Monad_Verify(verify)
     ,Verified
        ,signed
        ,original )
import Hetcons.Value
    ( Value
     ,Contains_Value
        ,extract_value
     ,Contains_1a
        ,extract_1a
     ,Contains_Quorums
        ,extract_observer_quorums
     ,Ballot
     ,Contains_Ballot
        ,extract_ballot
     ,Contains_1bs
        ,extract_1bs
    )

import Charlotte_Types
    ( Participant_ID(participant_ID_crypto_id)
     ,Invalid_Phase_2b(invalid_Phase_2b_explanation
                      ,invalid_Phase_2b_offending_phase_2b)
     ,Signed_Hash(signed_Hash_crypto_id)
     ,Signed_Message(signed_Message_signature)
     ,Phase_2b(phase_2b_phase_1bs)
              ,encode_Phase_2b
     ,default_Phase_2b
     ,default_Invalid_Phase_2b )

import Control.Monad.Except ( MonadError(throwError) )
import Data.Either.Combinators ()
import Data.Foldable ( Foldable(length) )
import Data.Hashable ( Hashable, hashWithSalt )
import Data.HashMap.Strict ( elems )
import Data.HashSet ( HashSet, unions, toList, intersection, fromList )
import qualified Data.HashSet as HashSet ( map )
import Data.List ( head )
import Data.Maybe ( catMaybes )
import Data.Text.Lazy ( pack )
import Data.Traversable ( mapM )
import Thrift.Protocol.Compact ( CompactProtocol(CompactProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-- | Encode a Phase_2b to a ByteString using Thrift
instance {-# OVERLAPPING #-} Encodable Phase_2b where
  encode = encode_Phase_2b (CompactProtocol EmptyTransport)


-- | We hasha Recursive_2b by hashing its non-recursive version
instance Hashable (Recursive_2b v) where
  hashWithSalt s (Recursive_2b x) = hashWithSalt s x

-- | A 2B contains 1Bs
instance {-# OVERLAPPING #-} (Value v) => Contains_1bs (Recursive_2b v) v where
  extract_1bs (Recursive_2b x) = x

-- | the 1A of a 2B message is the latest 1A (ballot number) present in all of its 1Bs
--   This is the same as the definition for 2As.
instance {-# OVERLAPPING #-} (Value v) => Contains_1a (Recursive_2b v) v where
  extract_1a (Recursive_2b x) = extract_1a $ Recursive_2a x

-- | A well-formed 2B features 1Bs all featuring the same value,
--   therefore the value of a 2B is the value of any of those 1Bs.
--   This is the same as the definition for 2As.
instance {-# OVERLAPPING #-} (Value v) => Contains_Value (Recursive_2b v) v where
  extract_value (Recursive_2b x) = extract_value $ Recursive_2a x

instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Ballot (Recursive_2b v) where
  extract_ballot x = extract_ballot ((extract_1a x) :: (Verified (Recursive_1a v)))

instance {-# OVERLAPPING #-} forall v . (Value v) => Contains_Quorums (Recursive_2b v) where
  extract_observer_quorums x = extract_observer_quorums ((extract_1a x) :: (Verified (Recursive_1a v)))

instance {-# OVERLAPPING #-} (Hashable v, Eq v, Value v, Monad_Verify (Recursive_1a v) m, Monad_Verify (Recursive_1b v) m) => From_Hetcons_Message (m (Recursive_2b v)) where
  from_Hetcons_Message x = do
    {(Recursive_2a r1bs) <- from_Hetcons_Message x
    ;return $ Recursive_2b r1bs
    }