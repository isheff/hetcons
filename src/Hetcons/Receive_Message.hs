{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Hetcons_Transaction is a Monad for constructing transactions in which a message is processed.
--   From within Hetcons_Transaction, you can send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
--   change the State.
--   You can also throw a Hetcons_Exception.
--   If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
module Hetcons.Receive_Message
  (Hetcons_Transaction
    ,run_Hetcons_Transaction_IO
    ,get_state
    ,put_state
    ,update_state
    ,get_my_crypto_id
    ,get_my_private_key
    ,get_witness
  ,Add_Sent
    ,add_sent
  ,Receivable
    ,receive
  ,Sendable
    ,send
  ,Hetcons_Server(Hetcons_Server)
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
  )
  where

import Hetcons.Hetcons_Exception ( Hetcons_Exception )
import Hetcons.Hetcons_State ( Hetcons_State, modify_and_read )
import Hetcons.Parsable (Parsable)
import Hetcons.Quorums (Monad_Verify_Quorums, verify_quorums, verify_quorums')
import Hetcons.Send_Message_IO ( send_Message_IO, Address_Book )
import Hetcons.Signed_Message
    ( Recursive_1b
     ,Recursive_1a
     ,Verified
     ,Recursive_2b
     ,Recursive_Proof_of_Consensus
     ,Recursive_2a
     ,Monad_Verify
       ,verify
       ,verify' )
import Hetcons.Value (Value)

import Charlotte_Types ( Crypto_ID, Signed_Message, Proposal_1a, Observers, Value_Witness )

import Control.Concurrent.Chan ( Chan )
import qualified Control.Concurrent.Map as CMap ( Map, lookup )
import Control.Concurrent.Map ( insertIfAbsent )
import Control.Concurrent.MVar ( MVar )
import Control.Exception.Base ( throw, catch )
import Control.Monad.Except ( throwError, catchError, MonadError )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Logger (MonadLogger, LoggingT, runChanLoggingT, unChanLoggingT, Loc, LogSource, LogLevel, LogStr)
import qualified Control.Monad.Parallel as Parallel ( sequence )
import Control.Monad.Reader ( MonadReader(reader, ask, local) )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT )
import Crypto.Random
    (drgNew, DRG(randomBytesGenerate), MonadRandom(getRandomBytes), getSystemDRG )
import Data.ByteString.Lazy ( ByteString )
import Data.Hashable ( Hashable )
import Data.HashSet ( HashSet, insert, toList, empty )
import Data.IORef
    ( IORef, writeIORef, readIORef, newIORef, atomicModifyIORef )
import Data.Serialize         (Serialize)
import Data.Tuple ( swap )

-- | Hetcons_Transaction is a Monad for constructing transactions in which a message is processed.
--   From within Hetcons_Transaction, you can send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
--   change the Participant_State.
--   You can also throw a Hetcons_Exception.
--   If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
--   It is constructed using the IO Monad, with a Reader Monad, so you can read environment veriables, that do stuff like
--    list the Var referencing State.
--   As a Newtype, you can't use, say, arbitrary IO stuff in this Monad, but only stuff exported in this submodule.
--   TODO: We may want to make this an adjective, like any Monad m can be instance Hetcons_Transaction s a m where ...
newtype Hetcons_Transaction s v a =
  Hetcons_Transaction {unwrap :: (
    LoggingT (
    ReaderT (Hetcons_Transaction_Environment s v)
    IO) a)
  } deriving (MonadLogger, Functor, Applicative, Monad, MonadReader (Hetcons_Transaction_Environment s v))



-- | Defines all the data that constitute a server, which maintains some state of type `s`:
--   This includes Memoization Caches
data (Hetcons_State s, Value v) => Hetcons_Server s v = Hetcons_Server {
  -- | The Server's Cryptographic ID (public key)
  hetcons_Server_crypto_id :: Crypto_ID
  -- | The Server's Private Key
 ,hetcons_Server_private_key :: ByteString
  -- | The Cache of the Server's open TCP connection handles to its neighbors
 ,hetcons_Server_address_book :: Address_Book
  -- | References the Server's mutable state
 ,hetcons_Server_state_var :: MVar s
  -- | The Memoization Cache for verifying 1As
 ,hetcons_Server_verify_1a :: CMap.Map Signed_Message (Verified (Recursive_1a v))
  -- | The Memoization Cache for verifying 1Bs
 ,hetcons_Server_verify_1b :: CMap.Map Signed_Message (Verified (Recursive_1b v))
  -- | The Memoization Cache for verifying 2As
 ,hetcons_Server_verify_2a :: CMap.Map Signed_Message (Verified (Recursive_2a v))
  -- | The Memoization Cache for verifying 2Bs
 ,hetcons_Server_verify_2b :: CMap.Map Signed_Message (Verified (Recursive_2b v))
  -- | The Memoization Cache for verifying Proof_of_Consensus
 ,hetcons_Server_verify_proof :: CMap.Map Signed_Message (Verified (Recursive_Proof_of_Consensus v))
  -- | The Memoization Cache for computing Quorums
 ,hetcons_Server_verify_quorums :: CMap.Map Proposal_1a Observers
  -- | The Channel input to the logger
 ,hetcons_Server_log_chan :: Chan (Loc, LogSource, LogLevel, LogStr)
}


-- | The immutable part of the Hetcons_Transaction Monad's state.
--   For instance, this is how we reference stuff in the Server's definition.
--   You can read this stuff anywhere in the Monad, but never change it.
data (Hetcons_State s, Value v) => Hetcons_Transaction_Environment s v = Hetcons_Transaction_Environment {
  -- | The data representing this server instance
  hetcons_Transaction_Environment_hetcons_server :: Hetcons_Server s v
  -- | A reference to this server's state
 ,hetcons_Transaction_Environment_transaction_state :: IORef (Hetcons_Transaction_State s v)
 -- | The "witness" from over the wire if this is a 1A or a 1B
 ,hetcons_Transaction_Environment_witness :: Value_Witness
}

-- | This is the internal state maintained by the Hetcons_Transaction Monad.
--   It tracks messages to be sent (recall that this Monad represents a transaction)
--   It also tracks what the new Participant_State should be.
--   This will be reset (except the hetcons_state) in each transaction.
data (Hetcons_State s, Value v) => Hetcons_Transaction_State s v = Hetcons_Transaction_State {
  -- | The 1As sent thus far in this transaction
  sent_1as :: HashSet (Verified (Recursive_1a v))
  -- | The 1Bs sent thus far in this transaction
 ,sent_1bs :: HashSet (Verified (Recursive_1b v))
  -- | The 2As sent thus far in this transaction
 ,sent_2as :: HashSet (Verified (Recursive_2a v))
  -- | The 2Bs sent thus far in this transaction
 ,sent_2bs :: HashSet (Verified (Recursive_2b v))
  -- | The Proof_of_Consensus s sent thus far in this transaction
 ,sent_Proof_of_Consensus :: HashSet (Verified (Recursive_Proof_of_Consensus v))
  -- | The state (possibly changed over the course of the transaction) of the server within the transaction.
  -- | The server's "real" state will be set to this at the end of the transaction, if no Errors are thrown.
 ,hetcons_state :: s
}

-- | MonadError instantiation, In which we basically catch and throw errors into the IO Monad.
--   TODO: This could probably be done with "deriving," except that for some reason we don't have (MonadIO m) => instance MonadError Hetcons_Exception m
instance (Hetcons_State s, Value v) => MonadError Hetcons_Exception (Hetcons_Transaction s v) where
  throwError = Hetcons_Transaction . throw
  catchError action handler = do { environment@Hetcons_Transaction_Environment{hetcons_Transaction_Environment_hetcons_server =
                                                 Hetcons_Server{hetcons_Server_log_chan = chan}} <- ask
                                 ; Hetcons_Transaction $ liftIO $ catch (runReaderT (runChanLoggingT chan (unwrap action)) environment)
                                                                        (\e -> runReaderT (runChanLoggingT chan $ unwrap (handler e)) environment)}

-- | When we want to getRandomBytes, we just call getSystemDRG down in the IO Monad
--   TODO: This could probably be done with "deriving," except that for some reason we don't have (MonadIO m) => instance MonadRandom m
instance (Hetcons_State s, Value v) => MonadRandom (Hetcons_Transaction s v) where
  getRandomBytes i = (Hetcons_Transaction $ liftIO getSystemDRG) >>= return . fst . (randomBytesGenerate i)



-- | Helper function.
--   Creates a monadic, memoized version of the given function, given:
--
--     * a function to memoize
--
--     * a field which pulls the memoization cache from the Hetcons_Server
memoize :: (Eq a, Hashable a, Hetcons_State s, Value v) => (a -> (Hetcons_Transaction s v b)) -> ((Hetcons_Server s v) -> (CMap.Map a b)) -> (a -> (Hetcons_Transaction s v b))
memoize f m x = do { table <- reader (m . hetcons_Transaction_Environment_hetcons_server)
                   ; cached <- Hetcons_Transaction $ liftIO $ CMap.lookup x table
                   ; case cached of
                       Just y -> return y
                       Nothing -> do { y <- f x
                                     ; Hetcons_Transaction $ liftIO $ insertIfAbsent x y table
                                     ; return y}}

-- | Memoization for verifying 1As in a Hetcons Transaction
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v, Parsable (Hetcons_Transaction s v v)) => Monad_Verify (Recursive_1a v) (Hetcons_Transaction s v) where
  verify = memoize verify' hetcons_Server_verify_1a

-- | Memoization for verifying 1Bs in a Hetcons Transaction
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction s v v)) => Monad_Verify (Recursive_1b v) (Hetcons_Transaction s v) where
  verify = memoize verify' hetcons_Server_verify_1b

-- | Memoization for verifying 2As in a Hetcons Transaction
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction s v v)) => Monad_Verify (Recursive_2a v) (Hetcons_Transaction s v) where
  verify = memoize verify' hetcons_Server_verify_2a

-- | Memoization for verifying 2Bs in a Hetcons Transaction
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction s v v)) => Monad_Verify (Recursive_2b v) (Hetcons_Transaction s v) where
  verify = memoize verify' hetcons_Server_verify_2b

-- | Memoization for verifying Proof_of_Consensus in a Hetcons Transaction
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v, Hashable v, Eq v, Parsable (Hetcons_Transaction s v v)) =>
                             Monad_Verify (Recursive_Proof_of_Consensus v) (Hetcons_Transaction s v) where
  verify = memoize verify' hetcons_Server_verify_proof

-- | Memoization for verifying Quorums in a Hetcons Transaction
instance {-# OVERLAPPING #-} (Hetcons_State s, Value v) => Monad_Verify_Quorums (Hetcons_Transaction s v) where
  verify_quorums = memoize verify_quorums' hetcons_Server_verify_quorums



-- | reads the current Hetcons_Transaction_State from the Monad's state
get_Hetcons_Transaction_State :: (Hetcons_State s, Value v) => Hetcons_Transaction s v (Hetcons_Transaction_State s v)
get_Hetcons_Transaction_State = do { transaction_state_ref <- Hetcons_Transaction $ reader hetcons_Transaction_Environment_transaction_state
                                   ; Hetcons_Transaction $ liftIO $ readIORef transaction_state_ref}


-- | writes the Hetcons_Transaction_State to the Monad's state
put_Hetcons_Transaction_State :: (Hetcons_State s, Value v) => (Hetcons_Transaction_State s v) -> Hetcons_Transaction s v ()
put_Hetcons_Transaction_State v = do { transaction_state_ref <- Hetcons_Transaction $ reader hetcons_Transaction_Environment_transaction_state
                                     ; Hetcons_Transaction $ liftIO $ writeIORef transaction_state_ref v}

-- | changes the current Hetcons_Transaction_State in the Monad's state, returning an extra value as well.
update_Hetcons_Transaction_State :: (Hetcons_State s, Value v) => ((Hetcons_Transaction_State s v) -> (a, (Hetcons_Transaction_State s v))) -> Hetcons_Transaction s v a
update_Hetcons_Transaction_State f = do { transaction_state_ref <- Hetcons_Transaction $ reader hetcons_Transaction_Environment_transaction_state
                                        ; Hetcons_Transaction $ liftIO $ atomicModifyIORef transaction_state_ref (swap . f)}

-- | reads the current Participant_State from the Monad's state
get_state :: (Hetcons_State s, Value v) => Hetcons_Transaction s v s
get_state = get_Hetcons_Transaction_State >>= (return . hetcons_state)

-- | writes the Participant_State to the Monad's state
put_state :: (Hetcons_State s, Value v) => s -> Hetcons_Transaction s v ()
put_state s = update_Hetcons_Transaction_State (\x -> ((), x{hetcons_state = s}))

-- | changes the current Participant_State in the Monad's state, returning an extra value as well.
update_state :: (Hetcons_State s, Value v) => (s -> (a, s)) -> Hetcons_Transaction s v a
update_state f = update_Hetcons_Transaction_State (\x -> let (y,z) = f $ hetcons_state x
                                                          in (y,x{hetcons_state = z}))

-- | Reads the Witness from the Monad
get_witness :: (Hetcons_State s, Value v) => Hetcons_Transaction s v Value_Witness
get_witness = reader hetcons_Transaction_Environment_witness

-- | Reades the Crypto_ID (public key) from the Monad
get_my_crypto_id :: (Hetcons_State s, Value v) => Hetcons_Transaction s v Crypto_ID
get_my_crypto_id = reader (hetcons_Server_crypto_id . hetcons_Transaction_Environment_hetcons_server)

-- | Reades the private key from  the Monad
get_my_private_key :: (Hetcons_State s, Value v) => Hetcons_Transaction s v ByteString
get_my_private_key = reader (hetcons_Server_private_key . hetcons_Transaction_Environment_hetcons_server)



-- | Given a Participant_State_Var which points to the current Participant_State,
--   atomically changes the Hetcons_Stae by running a Hetcons_Transaction.
--   This will then send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
--   return the returned value into the IO Monad.
--   You can also throw a Hetcons_Exception.
--   If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
run_Hetcons_Transaction_IO :: (Hetcons_State s, Value v) =>
  (Hetcons_Server s v) -> ((Verified (Recursive_Proof_of_Consensus v)) -> IO ()) -> Value_Witness -> (Hetcons_Transaction s v a) -> IO a
run_Hetcons_Transaction_IO (server@Hetcons_Server{hetcons_Server_log_chan = log_chan}) do_on_consensus witness receive_message =
  do { (answer, final_state) <- modify_and_read (hetcons_Server_state_var server)
                                  (\start_state -> do { start_transaction_state <- newIORef (
                                                          Hetcons_Transaction_State {
                                                             sent_1as = empty
                                                            ,sent_1bs = empty
                                                            ,sent_2as = empty
                                                            ,sent_2bs = empty
                                                            ,sent_Proof_of_Consensus = empty
                                                            ,hetcons_state = start_state})
                                                      ; let env =  Hetcons_Transaction_Environment {
                                                                      hetcons_Transaction_Environment_hetcons_server = server
                                                                     ,hetcons_Transaction_Environment_transaction_state = start_transaction_state
                                                                     ,hetcons_Transaction_Environment_witness = witness}
                                                      ; runReaderT (runChanLoggingT log_chan $ unwrap (do
                                                          { answer <- receive_message
                                                          ; final_transaction_state <- get_Hetcons_Transaction_State
                                                          ; final_state <- get_state
                                                          ; return (final_state, (answer, final_transaction_state))}))
                                                          env})
       -- as it is conceivable that sending messages could take arbitrarily long, we do so in parallel.
     ; Parallel.sequence ((map (send_Message_IO (hetcons_Server_address_book server) witness) $ toList $ sent_1as final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server) witness) $ toList $ sent_1bs final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server) witness) $ toList $ sent_2as final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server) witness) $ toList $ sent_2bs final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server) witness) $ toList $ sent_Proof_of_Consensus final_state)++
                          (map do_on_consensus $ toList $ sent_Proof_of_Consensus final_state))
     ; return answer}


-- | Class of types which can be sent as messages from within a Hetcons_Transaction monad.
class (Value v) => Add_Sent a v where
  -- | Adds a message to the set of outgoing messages in this Monadic transaction.
  --   This is intended to be used from within the `send` function.
  --   Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
  add_sent :: (Hetcons_State s) => a -> Hetcons_Transaction s v ()

-- | Adds a Proposal_1a to the set of outgoing messages in this Monadic transaction.
--   This is intended to be used from within the `send` function.
--   Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance (Value v) => Add_Sent (Verified (Recursive_1a v)) v where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_1as = insert p $ sent_1as x}))

-- | Adds a Phase_1b to the set of outgoing messages in this Monadic transaction.
--   This is intended to be used from within the `send` function.
--   Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance (Value v) => Add_Sent (Verified (Recursive_1b v)) v where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_1bs = insert p $ sent_1bs x}))

-- | Adds a Phase_2a to the set of outgoing messages in this Monadic transaction.
--   This is intended to be used from within the `send` function.
--   Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance (Value v) => Add_Sent (Verified (Recursive_2a v)) v where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_2as = insert p $ sent_2as x}))

-- | Adds a Phase_2b to the set of outgoing messages in this Monadic transaction.
--   This is intended to be used from within the `send` function.
--   Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance (Value v) => Add_Sent (Verified (Recursive_2b v)) v where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_2bs = insert p $ sent_2bs x}))

-- | Adds a Proof_of_Consensus to the set of outgoing messages in this Monadic transaction.
--   This is intended to be used from within the `send` function.
--   Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance (Value v) => Add_Sent (Verified (Recursive_Proof_of_Consensus v)) v where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_Proof_of_Consensus = insert p $ sent_Proof_of_Consensus x}))




-- | If you want to be able to receive a message and trigger a Monadic transaction (so, all the messages), this is what you implement.
class (Hetcons_State s, Value v) => Receivable s v a where
  -- | Implement receive to dictate what to do when a message comes in.
  receive :: a -> Hetcons_Transaction s v ()


-- | Those messages which you can send out from within a Monadic transaction are Sendable
class (Hetcons_State s, Value v) => Sendable s v a where
  -- | send a message from within a monadic transaction
  send :: a -> Hetcons_Transaction s v ()
