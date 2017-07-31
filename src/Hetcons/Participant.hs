{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hetcons.Participant (Participant, new_participant, participant_server, basic_participant_server, current_nanoseconds ) where

import Hetcons.Contains_Value ( Contains_1a, extract_1a )
import Hetcons.Hetcons_State ( Participant_State, start_State )
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
     ,Receivable(receive)
     ,run_Hetcons_Transaction_IO )
import Hetcons.Send_Message_IO ( default_Address_Book )
import Hetcons.Signed_Message
    ( Recursive_1b
     ,Verified
     ,Recursive_1a
     ,Recursive(non_recursive)
     ,Recursive_Proof_of_Consensus
     ,Monad_Verify(verify)
     ,original )
import Hetcons_Participant ( process )
import Hetcons_Participant_Iface
    ( Hetcons_Participant_Iface, ping, proposal_1a, phase_1b )
import Hetcons_Types
    ( Proposal_1a(proposal_1a_timestamp), Crypto_ID, Timestamp )
import Control.Concurrent ( forkIO, ThreadId, threadDelay )
import qualified Control.Concurrent.Map as CMap ( empty )
import Data.ByteString.Lazy ( ByteString )
import Data.Thyme.Clock
    ( getCurrentTime, UTCTime, UTCView(UTCTime) )
import Data.Thyme.Time.Core
    ( toThyme
     ,fromThyme
     ,diffUTCTime
     ,toMicroseconds
     ,fromMicroseconds
     ,fromGregorian )
import Thrift.Server ( runBasicServer )

type Participant = Hetcons_Server Participant_State

new_participant :: Crypto_ID -> ByteString -> IO Participant
new_participant cid pk =
  do { ab <- default_Address_Book
     ; sv <- start_State
     ; v1a <- CMap.empty
     ; v1b <- CMap.empty
     ; v2a <- CMap.empty
     ; v2b <- CMap.empty
     ; vproof <- CMap.empty
     ; vq <- CMap.empty
     ;return (Hetcons_Server {
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
           })}

participant_server :: (Integral a) => Participant -> a -> IO ThreadId
participant_server participant port = forkIO $ runBasicServer participant process $ fromIntegral port

basic_participant_server :: (Integral a) => Crypto_ID -> ByteString -> a -> IO ThreadId
basic_participant_server cid pk port = do { participant <- new_participant cid pk
                                          ; participant_server participant port}


-- | The current time, in nanoseconds since 1970 began.
-- | Of course, our library only actually does microseconds, so we're multiplying by 1000
current_nanoseconds :: IO Timestamp
current_nanoseconds = do { now <- getCurrentTime
                         ; return $ 1000 * (toMicroseconds $ diffUTCTime now ((toThyme $ fromThyme (UTCTime (fromGregorian 1970 0 0) (fromMicroseconds 0))) :: UTCTime))}

-- | the timestamp contained in (the proposal of) this message
message_timestamp :: (Contains_1a a) => a -> Timestamp
message_timestamp = proposal_1a_timestamp . non_recursive . original . extract_1a

-- | delay this thread by some number of nanoseconds
-- | note that the library we're using actually works in microseconds, so we're dividing by 1000, rounding down
delay_nanoseconds :: Timestamp -> IO ()
delay_nanoseconds = threadDelay . floor . (/1000) . fromIntegral

-- | delay this thread until the current time in nanoseconds is at least that of the timestamp in the message
delay_message :: (Contains_1a a) => a -> IO ()
delay_message m = do { now <- current_nanoseconds
                     ; delay_nanoseconds (now - (message_timestamp m))}

on_consensus :: (Verified Recursive_Proof_of_Consensus) -> IO ()
on_consensus = error . ("Somehow a Participant Proved Consensus: \n" ++) . show

instance Hetcons_Participant_Iface Participant where
  ping _ = return ()

  proposal_1a participant message
    = do { (verified :: (Verified Recursive_1a)) <- run_Hetcons_Transaction_IO participant on_consensus $ verify message -- TODO: this doesn't technically need a TX
         ; delay_message verified
         ; run_Hetcons_Transaction_IO participant on_consensus $ receive verified}

  phase_1b participant message
    = do { (verified :: (Verified Recursive_1b)) <- run_Hetcons_Transaction_IO participant on_consensus $ verify message -- TODO: this doesn't technically need a TX
         ; delay_message verified
         ; run_Hetcons_Transaction_IO participant on_consensus $ receive verified}
