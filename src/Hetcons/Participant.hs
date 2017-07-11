{-# LANGUAGE ScopedTypeVariables #-}

module Hetcons.Participant (Participant, new_participant, basic_participant_server, current_nanoseconds ) where

import Hetcons.Conflicting_2as    (conflicting_2as)
import Hetcons.Contains_Value     (Contains_1a, Contains_1bs, extract_1bs, extract_1a, extract_value, extract_ballot, extract_observer_quorums)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Hetcons_State      (Hetcons_State, Participant_State, Observer_State, Participant_State_Var, modify_and_read, default_State, start_State)
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
import Hetcons_Participant (process)
import Hetcons_Participant_Iface (Hetcons_Participant_Iface
                                   ,ping
                                   ,proposal_1a
                                   ,phase_1b)
import Hetcons_Types              (Timestamp
                                  ,Crypto_ID
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
                                  ,proposal_1a_timestamp
                                  )

import Control.Concurrent (forkIO, ThreadId, threadDelay)
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
import Data.Thyme.Clock           (getCurrentTime, UTCTime, UTCView(UTCTime))
import Data.Thyme.Time.Core       (toThyme, fromThyme, diffUTCTime, toMicroseconds, fromMicroseconds, fromGregorian)
import Thrift.Server (runBasicServer)

data Participant = Participant {
  crypto_id :: Crypto_ID
 ,private_key :: ByteString
 ,address_book :: Address_Book
 ,state_var :: Participant_State_Var
}

new_participant :: Crypto_ID -> ByteString -> IO Participant
new_participant cid pk =
  do { ab <- default_Address_Book
     ; sv <- start_State
     ; return Participant {
            crypto_id = cid
           ,private_key = pk
           ,address_book = ab
           ,state_var = sv}}

basic_participant_server :: (Integral a) => Crypto_ID -> ByteString -> a -> IO ThreadId
basic_participant_server cid pk port = forkIO (do { participant <- new_participant cid pk
                                                  ; runBasicServer participant process (fromIntegral port)})


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

  proposal_1a participant@(Participant {
                              crypto_id = cid
                             ,private_key = pk
                             ,address_book = ab
                             ,state_var = sv})
              message
    = case verify message of
        Left e -> throw e
        Right (verified :: (Verified Recursive_1a)) -> (delay_message verified) >> (run_Hetcons_Transaction_IO cid pk ab sv on_consensus $ receive verified)

  phase_1b participant@(Participant {
                           crypto_id = cid
                          ,private_key = pk
                          ,address_book = ab
                          ,state_var = sv})
           message
    = case verify message of
        Left e -> throw e
        Right (verified :: (Verified Recursive_1b)) -> (delay_message verified) >> (run_Hetcons_Transaction_IO cid pk ab sv on_consensus $ receive verified)
