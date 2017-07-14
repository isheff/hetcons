{-# LANGUAGE ScopedTypeVariables #-}

module Hetcons.Observer (Observer(Observer), do_on_consensus, observer_hetcons_server, new_observer, basic_observer_server, basic_observer_server_print, observer_server) where

import Hetcons.Conflicting_2as    (conflicting_2as)
import Hetcons.Contains_Value     (Contains_1bs, extract_1bs, extract_1a, extract_value, extract_ballot, extract_observer_quorums)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Hetcons_State      (Hetcons_State, Participant_State, Observer_State, Observer_State_Var, modify_and_read, default_State, start_State)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus (observers_proven)
import Hetcons.Receive()
import Hetcons.Receive_Message
  (Hetcons_Transaction
    ,run_Hetcons_Transaction_IO
    ,get_state
    ,put_state
    ,update_state
    ,get_my_crypto_id
    ,get_my_private_key
  ,Add_Sent
    ,add_sent
  ,Receivable
    ,receive
  ,Sendable
    ,send
  ,Hetcons_Server(Hetcons_Server)
    ,hetcons_Server_crypto_id
    ,hetcons_Server_private_key ,hetcons_Server_address_book ,hetcons_Server_state_var ,hetcons_Server_verify_1a ,hetcons_Server_verify_1b
    ,hetcons_Server_verify_2a
    ,hetcons_Server_verify_2b
    ,hetcons_Server_verify_proof
    ,hetcons_Server_verify_quorums
  )
import Hetcons.Send               ()
import Hetcons.Send_Message_IO    (Address_Book, default_Address_Book, send_Message_IO, domain_name)
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
                                  ,address_port_number
                                  ,participant_ID_address
                                  )

import Control.Concurrent (forkIO, ThreadId)
import qualified Control.Concurrent.Map as CMap (Map, empty, lookup)
import Control.Exception.Base     (throw)
import Control.Monad              (mapM, mapM_)
import Control.Monad.Except       (throwError, catchError, MonadError)
import Control.Monad.Reader       (MonadReader, Reader, runReader, reader, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.State        (StateT, runStateT, get, put,state)
import Crypto.Random              (drgNew)
import Data.ByteString.Lazy       (ByteString)
import Data.HashSet               (HashSet, insert, toList, fromList,  empty, member)
import qualified Data.HashSet as HashSet (map, filter)
import Data.Serialize             (Serialize)
import Thrift.Server (runBasicServer)

data Observer = Observer {
  observer_hetcons_server :: Hetcons_Server Observer_State
 ,do_on_consensus :: (Verified Recursive_Proof_of_Consensus) -> IO ()
}

new_observer :: Crypto_ID -> ByteString -> ((Verified Recursive_Proof_of_Consensus) -> IO ()) -> IO Observer
new_observer cid pk doc =
  do { ab <- default_Address_Book
     ; sv <- start_State
     ; v1a <- CMap.empty
     ; v1b <- CMap.empty
     ; v2a <- CMap.empty
     ; v2b <- CMap.empty
     ; vproof <- CMap.empty
     ; vq <- CMap.empty
     ; return Observer {
           observer_hetcons_server = (Hetcons_Server {
                                       hetcons_Server_crypto_id = cid
                                      ,hetcons_Server_private_key = pk
                                      ,hetcons_Server_address_book = ab
                                      ,hetcons_Server_state_var = sv
                                      ,hetcons_Server_verify_1a = v1a
                                      ,hetcons_Server_verify_1b = v1b
                                      ,hetcons_Server_verify_2a = v2a
                                      ,hetcons_Server_verify_2b = v2b
                                      ,hetcons_Server_verify_proof = vproof
                                      ,hetcons_Server_verify_quorums = vq
                                      })
           ,do_on_consensus = doc}}

basic_observer_server_print :: (Integral a) => Crypto_ID -> ByteString -> a -> IO ThreadId
basic_observer_server_print cid pk port = basic_observer_server cid pk port on_consensus

observer_server :: (Integral a) => Observer -> a -> IO ThreadId
observer_server observer port = forkIO $ runBasicServer observer process $ fromIntegral port

basic_observer_server :: (Integral a) => Crypto_ID -> ByteString -> a -> ((Verified Recursive_Proof_of_Consensus) -> IO ()) -> IO ThreadId
basic_observer_server cid pk port doc = do { observer <- new_observer cid pk doc
                                           ; observer_server observer port}

on_consensus :: (Verified Recursive_Proof_of_Consensus) -> IO ()
on_consensus proof = putStrLn $
  foldr (\n x -> x ++ "     " ++ (domain_name n) ++ " : "++ (show $ address_port_number $ participant_ID_address n) ++"\n")
        "\nCONSENSUS PROVEN for observers:\n"
        $ observers_proven proof



instance Hetcons_Observer_Iface Observer where
  ping _ = return ()

  phase_2b observer@(Observer {
                        observer_hetcons_server = s
                       ,do_on_consensus = doc})
              message
    = run_Hetcons_Transaction_IO s doc (do { (verified :: (Verified Recursive_2b)) <-  verify message -- TODO: this doesn't technically need to be in the TX
                                           ; receive verified})
