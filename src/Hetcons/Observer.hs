{-# LANGUAGE ScopedTypeVariables #-}

module Hetcons.Observer (Observer, new_observer, basic_observer_server) where

import Hetcons.Conflicting_2as    (conflicting_2as)
import Hetcons.Contains_Value     (Contains_1bs, extract_1bs, extract_1a, extract_value, extract_ballot, extract_observer_quorums)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Hetcons_State      (Hetcons_State, Participant_State, Observer_State, Observer_State_Var, modify_and_read, default_State, start_State)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Receive()
import Hetcons.Receive_Message
  (Hetcons_Transaction
    ,run_Hetcons_Transaction_IO
    ,get_state
    ,put_state
    ,update_state
    ,get_my_crypto_id
    ,get_my_private_key
    ,with_errors
  ,Add_Sent
    ,add_sent
  ,Receivable
    ,receive
  ,Sendable
    ,send)
import Hetcons.Send               ()
import Hetcons.Send_Message_IO    (Address_Book, default_Address_Book, send_Message_IO)
import Hetcons.Signed_Message     (Verified, original, signed, sign, verify, non_recursive, Recursive_1a, Recursive_1b, Recursive_2a, Recursive_2b, Recursive_Proof_of_Consensus, Parsable)

import Hetcons_Consts             (sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR)
import Hetcons_Observer (process)
import Hetcons_Observer_Iface (Hetcons_Observer_Iface
                                   ,ping
                                   ,phase_2b)
import Hetcons_Types              (Crypto_ID
                                  ,default_Phase_1b
                                  ,phase_1b_proposal
                                  ,phase_1b_conflicting_phase2as
                                  ,Signed_Message
                                  ,default_Phase_2a
                                  ,Phase_2a(Phase_2a)
                                    ,phase_2a_phase_1bs
                                  ,default_Phase_2b
                                  ,phase_2b_phase_1bs
                                  ,default_Proof_of_Consensus
                                  ,proof_of_Consensus_phase_2bs
                                  ,signed_Hash_crypto_id
                                  ,signed_Message_signature
                                  )

import Control.Concurrent (forkIO, ThreadId)
import Control.Exception.Base     (throw)
import Control.Monad              (mapM, mapM_)
import Control.Monad.Except       (throwError, catchError, MonadError)
import Control.Monad.Reader       (MonadReader, Reader, runReader, reader, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.State        (StateT, runStateT, get, put,state)
import Crypto.Random              (drgNew)
import Data.ByteString.Lazy       (ByteString)
import Data.Foldable              (maximum)
import Data.HashSet               (HashSet, insert, toList, fromList,  empty, member)
import qualified Data.HashSet as HashSet (map, filter)
import Data.Serialize             (Serialize)
import Thrift.Server (runBasicServer)

data Observer = Observer {
  crypto_id :: Crypto_ID
 ,private_key :: ByteString
 ,address_book :: Address_Book
 ,state_var :: Observer_State_Var
}

new_observer :: Crypto_ID -> ByteString -> IO Observer
new_observer cid pk =
  do { ab <- default_Address_Book
     ; sv <- start_State
     ; return Observer {
            crypto_id = cid
           ,private_key = pk
           ,address_book = ab
           ,state_var = sv}}

basic_observer_server :: (Integral a) => Crypto_ID -> ByteString -> a -> IO ThreadId
basic_observer_server cid pk port = forkIO (do { observer <- new_observer cid pk
                                               ; runBasicServer observer process (fromIntegral port)})




instance Hetcons_Observer_Iface Observer where
  ping _ = return ()

  phase_2b observer@(Observer {
                        crypto_id = cid
                       ,private_key = pk
                       ,address_book = ab
                       ,state_var = sv})
              message
    = case verify message of
        Left e -> throw e
        Right (verified :: (Verified Recursive_2b)) -> run_Hetcons_Transaction_IO cid pk ab sv $ receive verified
