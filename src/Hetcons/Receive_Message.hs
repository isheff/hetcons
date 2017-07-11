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
    ,send)
  where

import Hetcons.Contains_Value     (Contains_1bs, extract_1bs)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Hetcons_State      (Hetcons_State, Participant_State, Participant_State_Var, modify_and_read)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Send_Message_IO    (send_Message_IO, Address_Book)
import Hetcons.Signed_Message     (Verified, original, Recursive_1a, Recursive_1b, Recursive_2a, Recursive_2b, Recursive_Proof_of_Consensus, Parsable)

import Hetcons_Types              (Crypto_ID)

import Control.Concurrent.STM     (TVar)
import Control.Exception.Base     (throw)
import Control.Monad              (mapM_)
import Control.Monad.Except       (throwError, catchError, MonadError)
import qualified Control.Monad.Parallel as Parallel (mapM_, sequence)
import Control.Monad.Reader       (MonadReader, Reader, runReader, reader, ask, local)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.State        (StateT, runStateT, get, put,state)
import Crypto.Random              (SystemDRG, getSystemDRG, MonadRandom, getRandomBytes, randomBytesGenerate)
import Data.ByteString.Lazy       (ByteString)
import Data.HashSet               (HashSet, insert, toList, empty)




-- | Hetcons_Transaction is a Monad for constructing transactions in which a message is processed.
-- | From within Hetcons_Transaction, you can send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | change the Participant_State.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
newtype Hetcons_Transaction s a =
  Hetcons_Transaction {unwrap :: (
    EitherT Hetcons_Exception
            (StateT (Hetcons_Transaction_State s)
                    (Reader Hetcons_Transaction_Environment))
            a)}


-- | The immutable part of the Hetcons_Transaction Monad's state.
-- | You can read this stuff anywhere in the Monad, but never change it.
data Hetcons_Transaction_Environment = Hetcons_Transaction_Environment {
  crypto_id :: Crypto_ID
 ,private_key :: ByteString
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
 ,system_drg :: SystemDRG
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
  throwError = Hetcons_Transaction . throwError
  catchError action handler = Hetcons_Transaction $ catchError (unwrap action) $ unwrap . handler

-- | within a Hetcons_Transaction Monad, you can call Crypto.Random.drgNew to get a new ChaChaDRG
instance (Hetcons_State s) => MonadRandom (Hetcons_Transaction s) where
  getRandomBytes i = do { state <- get_Hetcons_Transaction_State
                        ; let (bytes, new_gen) = randomBytesGenerate i $ system_drg state
                        ; put_Hetcons_Transaction_State (state {system_drg = new_gen})
                        ; return bytes}

-- | within a Hetcons_Transaction Monad, you can call Control.Monad.Reader.ask to get the Hetcons_Transaction_Environment
instance (Hetcons_State s) => MonadReader Hetcons_Transaction_Environment (Hetcons_Transaction s) where
  ask = Hetcons_Transaction ask
  local f x = Hetcons_Transaction $ local f $ unwrap x

-- | Given a Hetcons_Transaction object, and a starting Hetcons_Transaction_State, this runs the Hetcons_Transaction object on that state, and returns the results.
-- | Results will either be a Hetcons_Exception or a final state and returned value.
run_Hetcons_Transaction :: (Hetcons_State s) =>
                           (Hetcons_Transaction s a) ->
                           Hetcons_Transaction_Environment ->
                           (Hetcons_Transaction_State s) ->
                           Either Hetcons_Exception (a, (Hetcons_Transaction_State s))
run_Hetcons_Transaction x v s = case (runReader (runStateT (runEitherT $ unwrap x) s) v) of
                              (Left  e, _) -> Left  e
                              (Right x, s) -> Right (x,s)


-- | Given a Participant_State_Var which points to the current Participant_State,
-- | atomically changes the Hetcons_Stae by running a Hetcons_Transaction.
-- | This will then send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | return the returned value into the IO Monad.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
run_Hetcons_Transaction_IO :: (Hetcons_State s) => Crypto_ID -> ByteString -> Address_Book ->  (TVar s) ->
                                                   ((Verified Recursive_Proof_of_Consensus) -> IO ()) -> (Hetcons_Transaction s a) -> IO a
run_Hetcons_Transaction_IO my_crypto_id my_private_key address_book state_var do_on_consensus receive_message =
  do { drg <- getSystemDRG
     ; let env = Hetcons_Transaction_Environment {crypto_id = my_crypto_id, private_key = my_private_key}
     ; f <- modify_and_read state_var
              (\start_state -> let final_state = run_Hetcons_Transaction receive_message env (
                                                   Hetcons_Transaction_State {
                                                     sent_1as = empty
                                                    ,sent_1bs = empty
                                                    ,sent_2as = empty
                                                    ,sent_2bs = empty
                                                    ,sent_Proof_of_Consensus = empty
                                                    ,hetcons_state = start_state
                                                    ,system_drg = drg
                                                   })
                                in case final_state of
                                     Left e -> (start_state, Left e)
                                     Right (x, final_receive_message_state) -> (hetcons_state final_receive_message_state, Right $ (final_receive_message_state,x)))
     ; case f of
         Left e -> throw e
         Right (final_receive_message_state, x) ->
           -- as it is conceivable that sending messages could take arbitrarily long, we do so in parallel.
           do { Parallel.sequence [Parallel.mapM_ (send_Message_IO address_book) $ toList $ sent_1as final_receive_message_state
                                  ,Parallel.mapM_ (send_Message_IO address_book) $ toList $ sent_1bs final_receive_message_state
                                  ,Parallel.mapM_ (send_Message_IO address_book) $ toList $ sent_2as final_receive_message_state
                                  ,Parallel.mapM_ (send_Message_IO address_book) $ toList $ sent_2bs final_receive_message_state
                                  ,Parallel.mapM_ (send_Message_IO address_book) $ toList $ sent_Proof_of_Consensus final_receive_message_state
                                  ,Parallel.mapM_ do_on_consensus $ toList $ sent_Proof_of_Consensus final_receive_message_state]
              ; return x}}

-- | reads the current Hetcons_Transaction_State from the Monad's state
get_Hetcons_Transaction_State :: (Hetcons_State s) => Hetcons_Transaction s (Hetcons_Transaction_State s)
get_Hetcons_Transaction_State = Hetcons_Transaction get

-- | writes the Hetcons_Transaction_State to the Monad's state
put_Hetcons_Transaction_State :: (Hetcons_State s) => (Hetcons_Transaction_State s) -> Hetcons_Transaction s ()
put_Hetcons_Transaction_State = Hetcons_Transaction . put

-- | changes the current Hetcons_Transaction_State in the Monad's state, returning an extra value as well.
update_Hetcons_Transaction_State :: (Hetcons_State s) => ((Hetcons_Transaction_State s) -> (a, (Hetcons_Transaction_State s))) -> Hetcons_Transaction s a
update_Hetcons_Transaction_State = Hetcons_Transaction . state

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
get_my_crypto_id = reader crypto_id

get_my_private_key :: (Hetcons_State s) => Hetcons_Transaction s ByteString
get_my_private_key = reader private_key

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
