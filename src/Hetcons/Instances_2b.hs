{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines the properties of 2B messages, most notably which typeclasses they're instances of
module Hetcons.Instances_2b () where

import Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
     ,Contains_1a
        ,extract_1a
        ,extract_observer_quorums
     ,Ballot
        ,extract_ballot
     ,Contains_1bs
        ,extract_1bs
    )
import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_Invalid_Phase_2b) )
import Hetcons.Instances_1b_2a ()
import Hetcons.Signed_Message
    ( Encodable
       ,encode
     ,Recursive_1b(recursive_1b_proposal)
     ,Parsable
       ,parse
     ,Recursive
       ,non_recursive
     ,Recursive_2b (Recursive_2b)
     ,Recursive_2a(Recursive_2a)
     ,Monad_Verify(verify)
     ,signed
     ,original )

import Hetcons_Types
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
import Data.HashSet ( unions, toList, intersection, fromList )
import qualified Data.HashSet as HashSet ( map )
import Data.List ( head )
import Data.Maybe ( catMaybes )
import Data.Text.Lazy ( pack )
import Data.Traversable ( mapM )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

-- | Encode a Phase_2b to a ByteString using Thrift
instance {-# OVERLAPPING #-} Encodable Phase_2b where
  encode = encode_Phase_2b (BinaryProtocol EmptyTransport)

-- | The Recursive version of a Phase_2b is a Recursive_2b
--   Phase_2b s carry signed 1b messages with them.
--   Recursive_2bs carry parsed and verified versions of these.
instance Recursive Phase_2b (Recursive_2b v) where
  non_recursive (Recursive_2b x) = default_Phase_2b {phase_2b_phase_1bs = HashSet.map signed x}

-- | We hasha Recursive_2b by hashing its non-recursive version
instance Hashable (Recursive_2b v) where
  hashWithSalt s (Recursive_2b x) = hashWithSalt s x

-- | A 2B contains 1Bs
instance {-# OVERLAPPING #-} Contains_1bs (Recursive_2b v) v where
  extract_1bs (Recursive_2b x) = x

-- | the 1A of a 2B message is the latest 1A (ballot number) present in all of its 1Bs
--   This is the same as the definition for 2As.
instance {-# OVERLAPPING #-} Contains_1a (Recursive_2b v) v where
  extract_1a (Recursive_2b x) = extract_1a $ Recursive_2a x

-- | A well-formed 2B features 1Bs all featuring the same value,
--   therefore the value of a 2B is the value of any of those 1Bs.
--   This is the same as the definition for 2As.
instance {-# OVERLAPPING #-} Contains_Value (Recursive_2b v) v where
  extract_value (Recursive_2b x) = extract_value $ Recursive_2a x


-- | Throws a Hetcons_Exception if this 2B is not well-formed.
--   A 2B is well-formed if it has all of:
--
--    * It has some 1Bs
--
--    * All 1Bs feature the same value
--
--    * All 1Bs feature Observers (we don't support not doing that)
--
--    * All 1Bs feature the same Observers
--
--    * The 1Bs satisfy at least one quorum of one Observer
well_formed_2b :: (Value v, MonadError Hetcons_Exception m) => (Recursive_2b v) -> m ()
well_formed_2b r2b@(Recursive_2b s) =
  do { if 1 /= (length $ HashSet.map extract_value s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2b (default_Invalid_Phase_2b{
                         invalid_Phase_2b_offending_phase_2b = non_recursive r2b
                        ,invalid_Phase_2b_explanation = Just $ pack "there were 1bs with different values in this 2b, or no 1bs at all"})
          else return ()
     ; if 1 /= (length $ HashSet.map extract_observer_quorums s)
          then throwError $ Hetcons_Exception_Invalid_Phase_2b (default_Invalid_Phase_2b{
                         invalid_Phase_2b_offending_phase_2b = non_recursive r2b
                        ,invalid_Phase_2b_explanation = Just $ pack "there were 1bs with different observers in this 2b"})
          else return ()
     ; let observers = extract_observer_quorums r2b
     ; if 0 == length observers
          then throwError $ Hetcons_Exception_Invalid_Phase_2b (default_Invalid_Phase_2b{
                         invalid_Phase_2b_offending_phase_2b = non_recursive r2b
                        ,invalid_Phase_2b_explanation = Just $ pack "at this time, we require that observer quorums be listed by participant ID"})
          else return ()
     ; let quorums_crypto_ids = HashSet.map (HashSet.map participant_ID_crypto_id) $ unions $ elems observers
     ; let crypto_ids_of_1bs = fromList $ catMaybes $ toList $ HashSet.map (signed_Hash_crypto_id . signed_Message_signature . signed) s
     ; if all (\q -> (q /= (intersection q crypto_ids_of_1bs))) quorums_crypto_ids
          then throwError $ Hetcons_Exception_Invalid_Phase_2b (default_Invalid_Phase_2b{
                         invalid_Phase_2b_offending_phase_2b = non_recursive r2b
                        ,invalid_Phase_2b_explanation = Just $ pack "this set of 1bs does not satisfy any known quorum"})
          else return ()
     }

-- | Parse a Recursive_2b (part of verifying it)
--   for a 2b message, we parse the original message, and verify the 1b messages it carries.
--   Also, we check its well-formed-ness
instance {-# OVERLAPPING #-} (Parsable v) => Parsable (Recursive_2b v) where
  parse payload =
    do { non_recursive <- parse payload
       ; l_set <- mapM verify $ toList $ phase_2b_phase_1bs non_recursive
       ; let set = fromList l_set
       ; if (length (HashSet.map (recursive_1b_proposal . original) set)) > 1
            then throwError $ Hetcons_Exception_Invalid_Phase_2b default_Invalid_Phase_2b {
                                 invalid_Phase_2b_offending_phase_2b = non_recursive
                                ,invalid_Phase_2b_explanation = Just "More than 1 proposal value present."}
            else return ()
       ; well_formed_2b $ Recursive_2b set
       ; return $ Recursive_2b set}
