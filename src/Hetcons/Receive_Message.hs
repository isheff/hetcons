{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Receive_Message is a Monad for constructing transactions in which a message is processed.
-- | From within Receive_Message, you can send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | change the Hetcons_State.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
module Hetcons.Receive_Message
  (Receive_Message
    ,run_Receive_Message_IO
    ,get_state
    ,put_state
    ,update_state
    ,add_sent_1a
    ,add_sent_1b
    ,add_sent_2a
    ,add_sent_2b
    ,add_sent_Proof_of_Consensus
  ,Receivable' -- This is the class you implement when you want something to be Receivable
    ,receive'
    ,receive -- Note that teh class Receivable is not exported. You cannot implement it without implementing Receivable'
  ,Sendable
  ,send)
  where

import Hetcons.Contains_Value     (Contains_1bs, extract_1bs)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Hetcons_State      (Hetcons_State, Hetcons_State_Var, modify_and_read, default_Hetcons_State)
import Hetcons.Send_Message_IO    (send_Message_IO)
import Hetcons.Signed_Message     (Verified, original, Recursive_1b, Parsable)

import Hetcons_Types              (Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus)

import Control.Exception.Base     (throw)
import Control.Monad              (mapM_)
import Control.Monad.Except       (throwError, catchError, MonadError)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.State        (State, runState, get, put,state)
import Crypto.Random              (SystemDRG, getSystemDRG, MonadRandom, getRandomBytes, randomBytesGenerate)
import Data.HashSet               (HashSet, insert, toList, empty)

-- | Receive_Message is a Monad for constructing transactions in which a message is processed.
-- | From within Receive_Message, you can send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | change the Hetcons_State.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
newtype Receive_Message a = Receive_Message {unwrap :: (EitherT Hetcons_Exception (State Receive_Message_State) a)}

-- | This is the internal state maintained by the Receive_Message Monad.
-- | It tracks messages to be sent (recall that this Monad represents a transaction)
-- | It also tracks what the new Hetcons_State should be.
data Receive_Message_State = Receive_Message_State {
  sent_1as :: HashSet Proposal_1a
 ,sent_1bs :: HashSet Phase_1b
 ,sent_2as :: HashSet Phase_2a
 ,sent_2bs :: HashSet Phase_2b
 ,sent_Proof_of_Consensus :: HashSet Proof_of_Consensus
 ,hetcons_state :: Hetcons_State
 ,system_drg :: SystemDRG
}

-- | The Receive_Message_State you start with, featuring a dummy value for the Hetcons_State
default_Receive_Message_State = Receive_Message_State {
  sent_1as = empty
 ,sent_1bs = empty
 ,sent_2as = empty
 ,sent_2bs = empty
 ,sent_Proof_of_Consensus = empty
 ,hetcons_state = default_Hetcons_State
}


-- | A general implementation of Functor which works for all Monads
instance Functor Receive_Message where
  fmap ab ma = ma >>= (return . ab) -- this is generally true of Monads

-- | Applicative, inherited from the EitherT Monad which Receive_Message wraps
instance Applicative Receive_Message where
  pure = Receive_Message . pure
  f <*> x = Receive_Message $ (unwrap f) <*> (unwrap x)

-- | Monad instantiation, inerited from the EitherT Monad which Receive_Message wraps
instance Monad Receive_Message where
  x >>= f = Receive_Message $ (unwrap x) >>= (unwrap . f)
  -- | fail Should really take advantage of Hetcons_Exception, but there's no specific exception that works here, since it's a very general failing...

-- | MonadError instantiation, inherited from the EitherT Monad which Receive_Message wraps.
instance MonadError Hetcons_Exception Receive_Message where
  throwError = Receive_Message . throwError
  catchError action handler = Receive_Message $ catchError (unwrap action) $ unwrap . handler

-- | within a Receive_Message Monad, you can call Crypto.Random.drgNew to get a new ChaChaDRG
instance MonadRandom Receive_Message where
  getRandomBytes i = do { state <- get_Receive_Message_State
                        ; let (bytes, new_gen) = randomBytesGenerate i $ system_drg state
                        ; put_Receive_Message_State (state {system_drg = new_gen})
                        ; return bytes}


-- | Given a Receive_Message object, and a starting Receive_Message_State, this runs the Receive_Message object on that state, and returns the results.
-- | Results will either be a Hetcons_Exception or a final state and returned value.
run_Receive_Message :: (Receive_Message a) -> Receive_Message_State ->  Either Hetcons_Exception (a, Receive_Message_State)
run_Receive_Message x s = case (runState (runEitherT $ unwrap x) s) of
                            (Left  e, _) -> Left  e
                            (Right x, s) -> Right (x,s)


-- | Given a Hetcons_State_Var which points to the current Hetcons_State,
-- | atomically changes the Hetcons_Stae by running a Receive_Message.
-- | This will then send messages (1a, 1b, 2a, 2b, proof_of_consensus), and
-- | return the returned value into the IO Monad.
-- | You can also throw a Hetcons_Exception.
-- | If an exception is thrown, it will be thrown in the IO monad, and NO STATE CHANGES WILL OCCUR, NO MESSAGES WILL BE SENT
run_Receive_Message_IO :: Hetcons_State_Var -> (Receive_Message a) -> IO a
run_Receive_Message_IO state_var receive_message =
  do { drg <- getSystemDRG
     ; f <- modify_and_read state_var
              (\start_state -> let final_state = run_Receive_Message receive_message (default_Receive_Message_State {hetcons_state = start_state, system_drg = drg})
                                in case final_state of
                                     Left e -> (start_state, Left e)
                                     Right (x, final_receive_message_state) -> (hetcons_state final_receive_message_state, Right $ (final_receive_message_state,x)))
     ; case f of
         Left e -> throw e
         Right (final_receive_message_state, x) ->
           do { mapM_ send_Message_IO $ toList $ sent_1as final_receive_message_state
              ; mapM_ send_Message_IO $ toList $ sent_1bs final_receive_message_state
              ; mapM_ send_Message_IO $ toList $ sent_2as final_receive_message_state
              ; mapM_ send_Message_IO $ toList $ sent_2bs final_receive_message_state
              ; mapM_ send_Message_IO $ toList $ sent_Proof_of_Consensus final_receive_message_state
              ; return x}}

-- | reads the current Receive_Message_State from the Monad's state
get_Receive_Message_State :: Receive_Message Receive_Message_State
get_Receive_Message_State = Receive_Message get

-- | writes the Receive_Message_State to the Monad's state
put_Receive_Message_State :: Receive_Message_State -> Receive_Message ()
put_Receive_Message_State = Receive_Message . put

-- | changes the current Receive_Message_State in the Monad's state, returning an extra value as well.
update_Receive_Message_State :: (Receive_Message_State -> (a, Receive_Message_State)) -> Receive_Message a
update_Receive_Message_State = Receive_Message . state

-- | reads the current Hetcons_State from the Monad's state
get_state :: Receive_Message Hetcons_State
get_state = get_Receive_Message_State >>= (return . hetcons_state)

-- | writes the Hetcons_State to the Monad's state
put_state :: Hetcons_State -> Receive_Message ()
put_state s = update_Receive_Message_State (\x -> ((), x{hetcons_state = s}))

-- | changes the current Hetcons_State in the Monad's state, returning an extra value as well.
update_state :: (Hetcons_State -> (a, Hetcons_State)) -> Receive_Message a
update_state f = update_Receive_Message_State (\x -> let (y,z) = f $ hetcons_state x
                                                      in (y,x{hetcons_state = z}))

-- | Adds a Proposal_1a to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
add_sent_1a :: Proposal_1a -> Receive_Message ()
add_sent_1a p = update_Receive_Message_State (\x -> ((),x{sent_1as = insert p $ sent_1as x}))

-- | Adds a Phase_1b to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
add_sent_1b :: Phase_1b -> Receive_Message ()
add_sent_1b p = update_Receive_Message_State (\x -> ((),x{sent_1bs = insert p $ sent_1bs x}))

-- | Adds a Phase_2a to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
add_sent_2a :: Phase_2a -> Receive_Message ()
add_sent_2a p = update_Receive_Message_State (\x -> ((),x{sent_2as = insert p $ sent_2as x}))

-- | Adds a Phase_2b to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
add_sent_2b :: Phase_2b -> Receive_Message ()
add_sent_2b p = update_Receive_Message_State (\x -> ((),x{sent_2bs = insert p $ sent_2bs x}))

-- | Adds a Proof_of_Consensus to the set of outgoing messages in this Monadic transaction.
-- | This is intended to be used from within the `send` function.
-- | Most of the time, you'll want to use `send`, which may have stuff to check to ensure everything's going correctly.
add_sent_Proof_of_Consensus :: Proof_of_Consensus -> Receive_Message ()
add_sent_Proof_of_Consensus p = update_Receive_Message_State (\x -> ((),x{sent_Proof_of_Consensus = insert p $ sent_Proof_of_Consensus x}))



-- | If you want to be able to receive a message and trigger a Monadic transaction (so, all the messages), this is what you implement.
class Receivable' a where
  -- | Implement receive' to dictate what to do when a message comes in.
  -- | However, you call receive (not receive'), when a message comes in.
  -- | The difference is that receive will automatically check for 1bs contained within this message, and receive those also.
  receive' :: a -> Receive_Message ()

-- | That which is Receivable' is also Receivable.
class Receivable a where
  -- | You call receive (not receive') when you get an incoming message.
  -- | The difference is that receive will automatically check for 1bs contained within this message, and receive those also.
  receive :: a -> Receive_Message ()

-- | By default, recieve is exactly receive'
instance {-# OVERLAPPABLE #-} (Receivable' a) => (Receivable a) where
  receive = receive'

-- | However, if there are 1bs contained within this message type, receive will extract them and receive them before you receive the contained message.
instance {-# OVERLAPPING #-} (Receivable (Verified Recursive_1b), Parsable a, Contains_1bs a, (Receivable' (Verified a))) => (Receivable (Verified a)) where
  receive x = (mapM_ receive $ toList $ extract_1bs $ original x) >> (receive' x)




-- | Those messages which you can send out from within a Monadic transaction are Sendable
class Sendable a where
  -- | send a message from within a monadic transaction
  send :: a -> Receive_Message ()
