{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | The Participant Server, which runs the Consensus algorithm, ultimately "accepting" and sending a 2B to Observer Servers
module Hetcons.Participant (Participant, new_participant, participant_server, basic_participant_server, current_nanoseconds ) where

import Hetcons.Compact_Server (runCompactServer)
import Hetcons.Hetcons_State ( Participant_State, start_State )
import Hetcons.Parsable (Parsable)
import Hetcons.Receive ()
import Hetcons.Receive_Message
    ( Hetcons_Server(Hetcons_Server)
        ,hetcons_Server_crypto_id
        ,hetcons_Server_private_key
        ,hetcons_Server_address_book
        ,hetcons_Server_state_var
        ,hetcons_Server_verify_bytestring
        ,hetcons_Server_verify_hetcons_message
        ,hetcons_Server_verify_1a
        ,hetcons_Server_verify_1b
        ,hetcons_Server_verify_2a
        ,hetcons_Server_verify_2b
        ,hetcons_Server_verify_proof
        ,hetcons_Server_verify_quorums
        ,hetcons_Server_log_chan
     ,Receivable(receive)
     ,run_Hetcons_Transaction_IO
     ,Hetcons_Transaction
    )
import Hetcons.Send_Message_IO ( default_Address_Book )
import Hetcons.Signed_Message
    ( Recursive_1b
     ,recursive_1a_non_recursive
     ,Verified
     ,Recursive_1a
     ,Recursive_Proof_of_Consensus
     ,Monad_Verify(verify)
     ,original )
import Hetcons.Value ( Value, Contains_1a, extract_1a )

import Hetcons_Participant ( process )
import Hetcons_Participant_Iface
    ( Hetcons_Participant_Iface, ping, proposal_1a, phase_1b )
import Charlotte_Types
    ( Proposal_1a(proposal_1a_timestamp), Crypto_ID, Timestamp, Slot_Value )

import Control.Concurrent ( forkIO, ThreadId, threadDelay )
import Control.Concurrent.Chan (newChan)
import qualified Control.Concurrent.Map as CMap ( empty )
import Control.Monad.Logger(unChanLoggingT, LoggingT, runStdoutLoggingT )
import Data.ByteString.Lazy ( ByteString )
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Thyme.Clock
    ( getCurrentTime, UTCTime, UTCView(UTCTime) )
import Data.Thyme.Time.Core
    ( toThyme
     ,fromThyme
     ,diffUTCTime
     ,toMicroseconds
     ,fromMicroseconds
     ,fromGregorian )

-- Janky way to disable debug statemetns instead of import Control.Monad.Logger.CallStack ( logDebugSH )
logDebugSH _ = return () 

-- | The participant Datum is just a Hetcons_Server with a Participant_State (defined in Hetcons_State)
type Participant v = Hetcons_Server (Participant_State v) v

-- | given a Cryptographic ID (public key), and a private key, produces a new Participant Datum
new_participant :: (Value v) => (LoggingT IO a -> IO a) -> Crypto_ID -> ByteString -> IO (Participant v)
new_participant run_logger cid pk =
  do { ab <- default_Address_Book
     ; sv <- start_State
     ; vb <- CMap.empty
     ; vh <- CMap.empty
     ; v1a <- CMap.empty
     ; v1b <- CMap.empty
     ; v2a <- CMap.empty
     ; v2b <- CMap.empty
     ; vproof <- CMap.empty
     ; vq <- CMap.empty
     ; chan <- newChan
     ; forkIO $ (run_logger $ unChanLoggingT chan) >> return ()
     ;return (Hetcons_Server {
            hetcons_Server_crypto_id = cid
           ,hetcons_Server_private_key = pk
           ,hetcons_Server_address_book = ab
           ,hetcons_Server_state_var = sv
           ,hetcons_Server_verify_bytestring = vb
           ,hetcons_Server_verify_hetcons_message = vh
           ,hetcons_Server_verify_1a = v1a
           ,hetcons_Server_verify_1b = v1b
           ,hetcons_Server_verify_2a = v2a
           ,hetcons_Server_verify_2b = v2b
           ,hetcons_Server_verify_proof = vproof
           ,hetcons_Server_verify_quorums = vq
           ,hetcons_Server_log_chan = chan
           })}

-- | Given a Participant Datum, and a Port Number, launches a Participant Server, and returns the ThreadId of that server.
participant_server :: (Value v, Show v, Eq v, Hashable v, Integral a, Parsable (Hetcons_Transaction (Participant_State v) v v)) => (Participant v) -> a -> IO ThreadId
participant_server participant port = forkIO $ runCompactServer participant process $ fromIntegral port

-- | Given a Cryptographic ID (public key), a private key, and a Port Number, launches a Participant Server, and returns the ThreadId of that server.
basic_participant_server :: (Integral a) => Crypto_ID -> ByteString -> a -> IO ThreadId
basic_participant_server cid pk port = do { (participant :: Participant Slot_Value) <- new_participant runStdoutLoggingT cid pk
                                          ; participant_server participant port}


-- | The current time, in nanoseconds since 1970 began.
--   Of course, our library only actually does microseconds, so we're multiplying by 1000
current_nanoseconds :: IO Timestamp
current_nanoseconds = do { now <- getCurrentTime
                         ; return $ 1000 * (toMicroseconds $ diffUTCTime now ((toThyme $ fromThyme (UTCTime (fromGregorian 1970 0 0) (fromMicroseconds 0))) :: UTCTime))}

-- | the timestamp contained in (the proposal of) this message
message_timestamp :: forall a v . (Value v, Contains_1a (Verified (a v)) v) => (Verified (a v)) -> Timestamp
message_timestamp = proposal_1a_timestamp . recursive_1a_non_recursive . original . (extract_1a :: (Verified (a v)) -> (Verified (Recursive_1a v)))

-- | delay this thread by some number of nanoseconds
--   note that the library we're using actually works in microseconds, so we're dividing by 1000, rounding down
delay_nanoseconds :: Timestamp -> IO ()
delay_nanoseconds = threadDelay . floor . (/1000) . fromIntegral

-- | delay this thread until the current time in nanoseconds is at least that of the timestamp in the message
delay_message :: (Contains_1a (Verified (a v)) v) => (Verified (a v)) -> IO ()
delay_message m = do { now <- current_nanoseconds
                     ; delay_nanoseconds (now - (message_timestamp m))}

-- | Technically, run_Hetcons_Transaction_IO needs something to do in the event a transaction returns a Proof_of_Consensus.
--   However, that should never happen on a Participant Server, so we just throw an error.
on_consensus :: (Show v) => (Verified (Recursive_Proof_of_Consensus v)) -> IO ()
on_consensus = error . ("Somehow a Participant Proved Consensus: \n" ++) . show

-- | Participant implements the Thrift Hetcons_Participant_Iface, meaning it acts as a Participant Server using the API defined in Thrift.
instance forall v . (Value v, Show v, Eq v, Hashable v, Parsable (Hetcons_Transaction (Participant_State v) v v)) => Hetcons_Participant_Iface (Participant v) where
  -- | When Pinged, a Participant returns ()
  ping _ = return ()

  -- | When it gets a 1A, the participant verifies it, delays it until our clock reaches its timestamp, and then runs `receive` (in a Hetcons_Transaction for atomicity)
  proposal_1a participant message witness
    = do { (verified :: (Verified (Recursive_1a v))) <- run_Hetcons_Transaction_IO participant on_consensus witness (do
                {logDebugSH message
                ;verified <- verify message
                ;logDebugSH verified
                ;return verified
                }) -- TODO: this doesn't need a TX
         ; delay_message verified
         -- ; run_Hetcons_Transaction_IO participant on_consensus witness $ logDebugSH verified
         -- ; delay_message verified
         ; run_Hetcons_Transaction_IO participant on_consensus witness $ receive verified
         }

  -- | When it gets a 1B, the participant verifies it, delays it until our clock reaches its timestamp, and then runs `receive` (in a Hetcons_Transaction for atomicity)
  phase_1b participant message witness
    = do { (verified :: (Verified (Recursive_1b v))) <- run_Hetcons_Transaction_IO participant on_consensus witness $ verify message -- TODO: this doesn't need a TX
         ; delay_message verified
         ; run_Hetcons_Transaction_IO participant on_consensus witness $ receive verified
         }
