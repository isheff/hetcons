{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The Observer Server, which waits to receive 2Bs from Participants, and produces Proof_of_Consensus when possible.
module Hetcons.Observer (Observer(Observer)
                          ,do_on_consensus
                          ,observer_hetcons_server
                          ,new_observer
                        ,basic_observer_server
                        ,basic_observer_server_print
                        ,observer_server)
where

import Hetcons.Hetcons_State ( Observer_State, start_State )
import Hetcons.Instances_Proof_of_Consensus ( observers_proven )
import Hetcons.Receive ()
import Hetcons.Receive_Message
    ( Hetcons_Server(Hetcons_Server)
       ,hetcons_Server_crypto_id
       ,hetcons_Server_private_key
       ,hetcons_Server_address_book
       ,hetcons_Server_state_var
       ,hetcons_Server_verify_1a
       ,hetcons_Server_verify_1b
       ,hetcons_Server_verify_2a
       ,hetcons_Server_verify_2b
       ,hetcons_Server_verify_proof
       ,hetcons_Server_verify_quorums
       ,hetcons_Server_log_chan
     ,Receivable(receive)
     ,Hetcons_Transaction
     ,run_Hetcons_Transaction_IO )
import Hetcons.Send_Message_IO ( domain_name, default_Address_Book )
import Hetcons.Signed_Message
    ( Verified
     ,Parsable
     ,Recursive_2b
     ,Recursive_Proof_of_Consensus
     ,Monad_Verify(verify) )
import Hetcons.Value (Value)

import Hetcons_Observer ( process )
import Hetcons_Observer_Iface
    ( Hetcons_Observer_Iface, ping, phase_2b )
import Charlotte_Types
    ( Participant_ID(participant_ID_address)
     ,Crypto_ID
     ,Slot_Value
     ,Address(address_port_number) )

import Control.Concurrent ( forkIO, ThreadId )
import Control.Concurrent.Chan (newChan)
import qualified Control.Concurrent.Map as CMap ( empty )
import Control.Monad.Logger(unChanLoggingT, LoggingT, runStdoutLoggingT )
import qualified Data.ByteString.Lazy as ByteString( empty )
import Data.ByteString.Lazy ( ByteString )
import Data.Hashable (Hashable)
import Thrift.Server ( runBasicServer )

-- | The Observer Server itself is defined as a data structure featuring:
--
--     * A Hetcons_Server whose state is an Observer_State (as defined in Hetcons_State)
--
--     * a function to execute whenever consensus is proven
--       (so it's given a Proof_of_Consensus)
data (Value v) => Observer v = Observer {
  observer_hetcons_server :: Hetcons_Server (Observer_State v) v
 ,do_on_consensus :: (Verified (Recursive_Proof_of_Consensus v)) -> IO ()
}

-- | Given a Cryptographic ID (public key), a private key, and a "Do on Consensus function", creates a new Observer datum.
--   This establishes default values for a bunch of stuff (mostly memoization caches), which are mostly the empty set.
new_observer :: (Value v) => (LoggingT IO a -> IO a) -> Crypto_ID -> ByteString -> ((Verified (Recursive_Proof_of_Consensus v)) -> IO ()) -> IO (Observer v)
new_observer run_logger cid pk doc =
  do { ab <- default_Address_Book
     ; sv <- start_State
     ; v1a <- CMap.empty
     ; v1b <- CMap.empty
     ; v2a <- CMap.empty
     ; v2b <- CMap.empty
     ; vproof <- CMap.empty
     ; vq <- CMap.empty
     ; chan <- newChan
     ; forkIO $ (run_logger $ unChanLoggingT chan) >> return ()
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
                                      ,hetcons_Server_log_chan = chan
                                      })
           ,do_on_consensus = doc}}

-- | Given an Observer Datum and a Port Number, boots up an Observer Server.
--   Returns the ThreadId of the newly started server.
observer_server :: (Integral a, Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v)) => (Observer v) -> a -> IO ThreadId
observer_server observer port = forkIO $ runBasicServer observer process $ fromIntegral port

-- | Given a Cryptographic ID (public key), a Private key, and a port number, and a "Do on Consensus function,"
--    boots up an Observer Server that runs that function on consensus.
--   Returns the ThreadId of the newly started server.
basic_observer_server :: (Integral a, Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v)) =>
                         Crypto_ID -> ByteString -> a -> ((Verified (Recursive_Proof_of_Consensus v)) -> IO ()) -> IO ThreadId
basic_observer_server cid pk port doc = do { observer <- new_observer runStdoutLoggingT cid pk doc
                                           ; observer_server observer port}

-- | Given a Cryptographic ID (public key), a Private key, and a port number, boots up an Observer Server that prints out when it's reached consensus.
--   Returns the ThreadId of the newly started server.
basic_observer_server_print :: (Integral a) => Crypto_ID -> ByteString -> a -> IO ThreadId
basic_observer_server_print cid pk port = basic_observer_server cid pk port (on_consensus :: (Verified (Recursive_Proof_of_Consensus Slot_Value)) -> IO ())

-- | The "Do on Consensus" function used by the basic_observer_server_print.
--   It prints out when Consensus is Profen, with a set of observers for whom it is proven.
on_consensus :: (Value v) => (Verified (Recursive_Proof_of_Consensus v)) -> IO ()
on_consensus proof = putStrLn $
  foldr (\n x -> x ++ "     " ++ (domain_name n) ++ " : "++ (show $ address_port_number $ participant_ID_address n) ++"\n")
        "\nCONSENSUS PROVEN for observers:\n"
        $ observers_proven proof

-- | Observer Data are instances of the Thrift Hetcons_Observer_Iface class, meaning they fulfil the requirements of the Thrift interface:
instance (Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction (Observer_State v) v v)) => Hetcons_Observer_Iface (Observer v) where
  -- | When pinged, the server returns ()
  ping _ = return ()

  -- | When it receives a Phase_2b, it verifies it, and runs the receive function in a Hetcons_Transaction (for atomicity)
  phase_2b observer@(Observer {
                        observer_hetcons_server = s
                       ,do_on_consensus = doc})
              message
    = run_Hetcons_Transaction_IO s doc ByteString.empty (do { (verified :: (Verified (Recursive_2b v))) <-  verify message -- TODO: this doesn't need to be in the TX
                                                        ; receive verified})
