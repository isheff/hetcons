{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Hetcons_Transaction is a Monad for constructing transactions in which a message is processed.
-- | From within Hetcons_Transaction, you can send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | change the Participant_State.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
module Hetcons.Receive_Message
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
    ,hetcons_Server_private_key
    ,hetcons_Server_address_book
    ,hetcons_Server_state_var
    ,hetcons_Server_verify_1a
    ,hetcons_Server_verify_1b
    ,hetcons_Server_verify_2a
    ,hetcons_Server_verify_2b
    ,hetcons_Server_verify_proof
    ,hetcons_Server_verify_quorums
  )
  where

import Hetcons.Contains_Value     (Contains_1bs, extract_1bs)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Hetcons_State      (Hetcons_State, Participant_State, Participant_State_Var, modify_and_read)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Quorums (Monad_Verify_Quorums, verify_quorums, verify_quorums')
import Hetcons.Send_Message_IO    (send_Message_IO, Address_Book)
import Hetcons.Signed_Message     (Monad_Verify, verify, verify', Verified, original, Recursive_1a, Recursive_1b, Recursive_2a, Recursive_2b, Recursive_Proof_of_Consensus, Parsable)

import Hetcons_Types              (Crypto_ID, Signed_Message, Proposal_1a, Observers)

import qualified Control.Concurrent.Map as CMap (Map, empty, lookup)
import Control.Concurrent.Map     (insertIfAbsent)
import Control.Concurrent.MVar    (MVar)
import Control.Exception.Base     (throw, catch)
import Control.Monad              (mapM_)
import Control.Monad.Except       (throwError, catchError, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Parallel as Parallel (mapM_, sequence)
import Control.Monad.Reader       (MonadReader, Reader, runReader, reader, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.State        (StateT, runStateT, get, put,state)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Crypto.Random              (SystemDRG, getSystemDRG, MonadRandom, getRandomBytes, randomBytesGenerate)
import Data.ByteString.Lazy       (ByteString)
import Data.HashSet               (HashSet, insert, toList, empty)
import Data.IORef                 (IORef, newIORef, readIORef, writeIORef, modifyIORef, atomicModifyIORef)
import Data.Tuple                 (swap)




-- | Hetcons_Transaction is a Monad for constructing transactions in which a message is processed.
-- | From within Hetcons_Transaction, you can send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | change the Participant_State.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
newtype Hetcons_Transaction s a =
  Hetcons_Transaction {unwrap :: (
    ReaderT (Hetcons_Transaction_Environment s)
    IO a)}



data (Hetcons_State s) => Hetcons_Server s = Hetcons_Server {
  hetcons_Server_crypto_id :: Crypto_ID
 ,hetcons_Server_private_key :: ByteString
 ,hetcons_Server_address_book :: Address_Book
 ,hetcons_Server_state_var :: MVar s
 ,hetcons_Server_verify_1a :: CMap.Map Signed_Message (Verified Recursive_1a)
 ,hetcons_Server_verify_1b :: CMap.Map Signed_Message (Verified Recursive_1b)
 ,hetcons_Server_verify_2a :: CMap.Map Signed_Message (Verified Recursive_2a)
 ,hetcons_Server_verify_2b :: CMap.Map Signed_Message (Verified Recursive_2b)
 ,hetcons_Server_verify_proof :: CMap.Map Signed_Message (Verified Recursive_Proof_of_Consensus)
 ,hetcons_Server_verify_quorums :: CMap.Map Proposal_1a Observers
}


-- | The immutable part of the Hetcons_Transaction Monad's state.
-- | You can read this stuff anywhere in the Monad, but never change it.
data (Hetcons_State s) => Hetcons_Transaction_Environment s = Hetcons_Transaction_Environment {
  hetcons_Transaction_Environment_hetcons_server :: Hetcons_Server s
 ,hetcons_Transaction_Environment_transaction_state :: IORef (Hetcons_Transaction_State s)
}

-- | This is the internal state maintained by the Hetcons_Transaction Monad.
-- | It tracks messages to be sent (recall that this Monad represents a transaction)
-- | It also tracks what the new Participant_State should be.
data (Hetcons_State s) => Hetcons_Transaction_State s = Hetcons_Transaction_State {
  sent_1as :: HashSet (Verified Recursive_1a)
 ,sent_1bs :: HashSet (Verified Recursive_1b)
 ,sent_2as :: HashSet (Verified Recursive_2a)
 ,sent_2bs :: HashSet (Verified Recursive_2b)
 ,sent_Proof_of_Consensus :: HashSet (Verified Recursive_Proof_of_Consensus)
 ,hetcons_state :: s
}




-- | A general implementation of Functor which works for all Monads
instance (Hetcons_State s) => Functor (Hetcons_Transaction s) where
  fmap ab ma = ma >>= (return . ab) -- this is generally true of Monads

-- | Applicative, inherited from the EitherT Monad which Hetcons_Transaction wraps
instance (Hetcons_State s) => Applicative (Hetcons_Transaction s) where
  pure = Hetcons_Transaction . pure
  f <*> x = Hetcons_Transaction $ (unwrap f) <*> (unwrap x)

-- | Monad instantiation, inerited from the EitherT Monad which Hetcons_Transaction wraps
instance (Hetcons_State s) => Monad (Hetcons_Transaction s) where
  x >>= f = Hetcons_Transaction $ (unwrap x) >>= (unwrap . f)
  -- | fail Should really take advantage of Hetcons_Exception, but there's no specific exception that works here, since it's a very general failing...

-- | MonadError instantiation, inherited from the EitherT Monad which Hetcons_Transaction wraps.
instance (Hetcons_State s) => MonadError Hetcons_Exception (Hetcons_Transaction s) where
  throwError = Hetcons_Transaction . throw
  catchError action handler = do { r <- ask
                                 ; Hetcons_Transaction $ liftIO $ catch (runReaderT (unwrap action) r) (\e -> runReaderT (unwrap (handler e)) r)}

-- | within a Hetcons_Transaction Monad, you can call Crypto.Random.drgNew to get a new ChaChaDRG
instance (Hetcons_State s) => MonadRandom (Hetcons_Transaction s) where
  getRandomBytes i = (Hetcons_Transaction $ liftIO getSystemDRG) >>= return . fst . (randomBytesGenerate i)

-- | within a Hetcons_Transaction Monad, you can call Control.Monad.Reader.ask to get the Hetcons_Transaction_Environment
instance (Hetcons_State s) => MonadReader (Hetcons_Transaction_Environment s) (Hetcons_Transaction s) where
  ask = Hetcons_Transaction ask
  local f x = Hetcons_Transaction $ local f $ unwrap x

-- TODO: there must be a more compact way to write the following:
instance {-# OVERLAPPING #-} (Hetcons_State s) => Monad_Verify Recursive_1a (Hetcons_Transaction s) where
  verify x = do { table <- reader (hetcons_Server_verify_1a . hetcons_Transaction_Environment_hetcons_server)
                ; cached <- Hetcons_Transaction $ liftIO $ CMap.lookup x table
                ; case cached of
                    Just y -> return y
                    Nothing -> do { y <- verify' x
                                  ; Hetcons_Transaction $ liftIO $ insertIfAbsent x y table
                                  ; return y}}

instance {-# OVERLAPPING #-} (Hetcons_State s) => Monad_Verify Recursive_1b (Hetcons_Transaction s) where
  verify x = do { table <- reader (hetcons_Server_verify_1b . hetcons_Transaction_Environment_hetcons_server)
                ; cached <- Hetcons_Transaction $ liftIO $ CMap.lookup x table
                ; case cached of
                    Just y -> return y
                    Nothing -> do { y <- verify' x
                                  ; Hetcons_Transaction $ liftIO $ insertIfAbsent x y table
                                  ; return y}}

instance {-# OVERLAPPING #-} (Hetcons_State s) => Monad_Verify Recursive_2a (Hetcons_Transaction s) where
  verify x = do { table <- reader (hetcons_Server_verify_2a . hetcons_Transaction_Environment_hetcons_server)
                ; cached <- Hetcons_Transaction $ liftIO $ CMap.lookup x table
                ; case cached of
                    Just y -> return y
                    Nothing -> do { y <- verify' x
                                  ; Hetcons_Transaction $ liftIO $ insertIfAbsent x y table
                                  ; return y}}

instance {-# OVERLAPPING #-} (Hetcons_State s) => Monad_Verify Recursive_2b (Hetcons_Transaction s) where
  verify x = do { table <- reader (hetcons_Server_verify_2b . hetcons_Transaction_Environment_hetcons_server)
                ; cached <- Hetcons_Transaction $ liftIO $ CMap.lookup x table
                ; case cached of
                    Just y -> return y
                    Nothing -> do { y <- verify' x
                                  ; Hetcons_Transaction $ liftIO $ insertIfAbsent x y table
                                  ; return y}}

instance {-# OVERLAPPING #-} (Hetcons_State s) => Monad_Verify Recursive_Proof_of_Consensus (Hetcons_Transaction s) where
  verify x = do { table <- reader (hetcons_Server_verify_proof . hetcons_Transaction_Environment_hetcons_server)
                ; cached <- Hetcons_Transaction $ liftIO $ CMap.lookup x table
                ; case cached of
                    Just y -> return y
                    Nothing -> do { y <- verify' x
                                  ; Hetcons_Transaction $ liftIO $ insertIfAbsent x y table
                                  ; return y}}

instance {-# OVERLAPPING #-} (Hetcons_State s) => Monad_Verify_Quorums (Hetcons_Transaction s) where
  verify_quorums x = do { table <- reader (hetcons_Server_verify_quorums . hetcons_Transaction_Environment_hetcons_server)
                        ; cached <- Hetcons_Transaction $ liftIO $ CMap.lookup x table
                        ; case cached of
                            Just y -> return y
                            Nothing -> do { y <- verify_quorums' x
                                          ; Hetcons_Transaction $ liftIO $ insertIfAbsent x y table
                                          ; return y}}



-- | reads the current Hetcons_Transaction_State from the Monad's state
get_Hetcons_Transaction_State :: (Hetcons_State s) => Hetcons_Transaction s (Hetcons_Transaction_State s)
get_Hetcons_Transaction_State = do { transaction_state_ref <- Hetcons_Transaction $ reader hetcons_Transaction_Environment_transaction_state
                                   ; Hetcons_Transaction $ liftIO $ readIORef transaction_state_ref}


-- | writes the Hetcons_Transaction_State to the Monad's state
put_Hetcons_Transaction_State :: (Hetcons_State s) => (Hetcons_Transaction_State s) -> Hetcons_Transaction s ()
put_Hetcons_Transaction_State v = do { transaction_state_ref <- Hetcons_Transaction $ reader hetcons_Transaction_Environment_transaction_state
                                     ; Hetcons_Transaction $ liftIO $ writeIORef transaction_state_ref v}

-- | changes the current Hetcons_Transaction_State in the Monad's state, returning an extra value as well.
update_Hetcons_Transaction_State :: (Hetcons_State s) => ((Hetcons_Transaction_State s) -> (a, (Hetcons_Transaction_State s))) -> Hetcons_Transaction s a
update_Hetcons_Transaction_State f = do { transaction_state_ref <- Hetcons_Transaction $ reader hetcons_Transaction_Environment_transaction_state
                                        ; Hetcons_Transaction $ liftIO $ atomicModifyIORef transaction_state_ref (swap . f)}

-- | reads the current Participant_State from the Monad's state
get_state :: (Hetcons_State s) => Hetcons_Transaction s s
get_state = get_Hetcons_Transaction_State >>= (return . hetcons_state)

-- | writes the Participant_State to the Monad's state
put_state :: (Hetcons_State s) => s -> Hetcons_Transaction s ()
put_state s = update_Hetcons_Transaction_State (\x -> ((), x{hetcons_state = s}))

-- | changes the current Participant_State in the Monad's state, returning an extra value as well.
update_state :: (Hetcons_State s) => (s -> (a, s)) -> Hetcons_Transaction s a
update_state f = update_Hetcons_Transaction_State (\x -> let (y,z) = f $ hetcons_state x
                                                          in (y,x{hetcons_state = z}))

get_my_crypto_id :: (Hetcons_State s) => Hetcons_Transaction s Crypto_ID
get_my_crypto_id = reader (hetcons_Server_crypto_id . hetcons_Transaction_Environment_hetcons_server)

get_my_private_key :: (Hetcons_State s) => Hetcons_Transaction s ByteString
get_my_private_key = reader (hetcons_Server_private_key . hetcons_Transaction_Environment_hetcons_server)



-- | Given a Participant_State_Var which points to the current Participant_State,
-- | atomically changes the Hetcons_Stae by running a Hetcons_Transaction.
-- | This will then send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | return the returned value into the IO Monad.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
run_Hetcons_Transaction_IO :: (Hetcons_State s) => (Hetcons_Server s) -> ((Verified Recursive_Proof_of_Consensus) -> IO ()) -> (Hetcons_Transaction s a) -> IO a
run_Hetcons_Transaction_IO server do_on_consensus receive_message =
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
                                                                     ,hetcons_Transaction_Environment_transaction_state = start_transaction_state}
                                                      ; runReaderT (unwrap (do { answer <- receive_message
                                                                               ; final_transaction_state <- get_Hetcons_Transaction_State
                                                                               ; final_state <- get_state
                                                                               ; return (final_state, (answer, final_transaction_state))}))
                                                                   env})
       -- as it is conceivable that sending messages could take arbitrarily long, we do so in parallel.
     ; Parallel.sequence ((map (send_Message_IO (hetcons_Server_address_book server)) $ toList $ sent_1as final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server)) $ toList $ sent_1bs final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server)) $ toList $ sent_2as final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server)) $ toList $ sent_2bs final_state)++
                          (map (send_Message_IO (hetcons_Server_address_book server)) $ toList $ sent_Proof_of_Consensus final_state)++
                          (map do_on_consensus $ toList $ sent_Proof_of_Consensus final_state))
     ; return answer}


class Add_Sent a where
  -- | Adds a message to the set of outgoing messages in this Monadic transaction.
  -- | This is intended to be used from within the `send` function.
  -- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
  add_sent :: (Hetcons_State s) => a -> Hetcons_Transaction s ()

-- | Adds a Proposal_1a to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance Add_Sent (Verified Recursive_1a) where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_1as = insert p $ sent_1as x}))

-- | Adds a Phase_1b to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance Add_Sent (Verified Recursive_1b) where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_1bs = insert p $ sent_1bs x}))

-- | Adds a Phase_2a to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance Add_Sent (Verified Recursive_2a) where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_2as = insert p $ sent_2as x}))

-- | Adds a Phase_2b to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance Add_Sent (Verified Recursive_2b) where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_2bs = insert p $ sent_2bs x}))

-- | Adds a Proof_of_Consensus to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
instance Add_Sent (Verified Recursive_Proof_of_Consensus) where
  add_sent p = update_Hetcons_Transaction_State (\x -> ((),x{sent_Proof_of_Consensus = insert p $ sent_Proof_of_Consensus x}))




-- | If you want to be able to receive a message and trigger a Monadic transaction (so, all the messages), this is what you implement.
class Receivable s a where
  -- | Implement receive to dictate what to do when a message comes in.
  receive :: (Hetcons_State s) => a -> Hetcons_Transaction s ()


-- | Those messages which you can send out from within a Monadic transaction are Sendable
class Sendable s a where
  -- | send a message from within a monadic transaction
  send :: a -> Hetcons_Transaction s ()
