{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Defines what can be a Server's State, and some utility functions for that.
module Hetcons.Hetcons_State
    ( Hetcons_State
    , Participant_State_Var
    , Participant_State
    , Observer_State_Var
    , Observer_State
    , default_State
    , conflicting_state
    , new_State
    , start_State
    , modify
    , read
    , modify_and_read
    , state_by_observers
    ) where

import Hetcons.Instances_1b_2a ()
import Hetcons.Signed_Message
    ( Recursive_1b
     ,Verified
     ,Recursive_2b )
import Hetcons.Value
    ( Contains_1a
     ,extract_observer_quorums
     ,Value
       ,garbage_collect
       ,conflicts
    )

import Control.Concurrent.MVar
    ( MVar
       ,modifyMVar_
       ,modifyMVar
       ,newMVar
       ,readMVar )
import Data.Foldable ( toList, any )
import qualified Data.HashSet as HashSet ( map, filter )
import Data.HashSet ( HashSet, fromList, empty )
import Data.Hashable ( Hashable )

import Prelude
    ( (.), (==), Bool, Foldable, IO, ($), return, Eq, id )


-- | To be a State type (the state kept by a server), you need a `write_prep` function, which is run on an object before saving it to state.
--   This is where, say, any garbage collection would go.
class Hetcons_State a where
  write_prep :: a -> a

-- | Participants store literally the set of 1b messages received or sent thus far (or at least those which have been verified)
type Participant_State v = HashSet (Verified (Recursive_1b v))
-- | Therefore, the `write_prep` for a `Participant_State` is `garbage_collect`, as defined in `Value`.
instance (Value v) => Hetcons_State (Participant_State v) where
  write_prep = garbage_collect

-- | Mutable references to Participant State that work in the Hetcons_Transaction monad.
type Participant_State_Var v = MVar (Participant_State v)



-- | Observers store the set of 2b messages received or sent thus far (or at least those which have been verified).
type Observer_State v = HashSet (Verified (Recursive_2b v))
-- | For now, the `write_prep` for an `Observer_State` is `id`, meaning it does nothing.
--   TODO: Can this be made more efficient? When can we delete 2bs from history?
instance Hetcons_State (Observer_State v) where
  write_prep = id

-- | Mutable references to Observer State that work in the Hetcons_Transaction monad.
type Observer_State_Var v = MVar (Observer_State v)


-- | The "Start" or "default" state for both Observers and Participants happens to be the empty set.
default_State :: (HashSet a)
default_State = empty


class State_by_Observers a where
  state_by_observers :: (Contains_1a (a v) v, Hashable (Verified (a v)), Eq (Verified (a v))) => (HashSet (Verified (a v))) -> (HashSet (HashSet (Verified (a v))))

-- | Subsets of the proposals which have the same Condensed Observer Graph, for each Condensed Observer Graph in the State.
--   strict superset of :: Participant_State -> (HashSet (Participant_State))
--                 and  ::    Observer_State -> (HashSet (   Observer_State))
instance State_by_Observers Recursive_1b where
  state_by_observers s = (HashSet.map (\x -> (HashSet.filter ((x ==) . extract_observer_quorums) s)) -- 1bs per COG
                                      (HashSet.map extract_observer_quorums s)) -- all the COGs
instance State_by_Observers Recursive_2b where
  state_by_observers s = (HashSet.map (\x -> (HashSet.filter ((x ==) . extract_observer_quorums) s)) -- 2bs per COG
                                      (HashSet.map extract_observer_quorums s)) -- all the COGs

-- | Does this proposal conflict with proposals in this state?
conflicting_state :: (Value v) => (Verified (Recursive_1b v)) -> (Participant_State v) -> Bool
conflicting_state = conflicts

-- | A reference to a new state containing all of the elements fo the given input
--   a strict superset of :: (Foldable t) => (t (Verified Recursive_1b)) -> IO Participant_State_Var
new_State :: (Foldable t, Hashable a, Eq a) => (t a) -> IO (MVar (HashSet a))
new_State = newMVar . fromList . toList

-- | a reference to a new, empty, state
--   a strict superset of :: IO Participant_State_Var
start_State :: (Hashable a, Eq a, Hetcons_State (HashSet a)) => IO (MVar (HashSet a))
start_State = new_State []

-- | Returns the present value of the mutable state reference given
--   a strict superset of :: Participant_State_Var -> IO Participant_State
read :: (MVar a) -> IO a
read = readMVar



-- | applies the given function to the state in the mutable state reference given (and then applies `write_prep`)
-- strict superset of :: Participant_State_Var -> (Participant_State -> Participant_State) -> IO ()
modify :: (Hetcons_State a) => (MVar a) -> (a -> a) -> IO ()
modify s f = modifyMVar_ s $ return . write_prep . f

-- | applies the given function to the state in the mutable state reference given (with `write_prep`), and also returns the second output of the function.
-- strict superset of ::  Participant_State_Var -> (Participant_State -> (Participant_State, a)) -> IO a
modify_and_read :: (Hetcons_State a) => (MVar a) -> (a -> (IO (a, b))) -> IO b
modify_and_read s f = modifyMVar s (\v -> do { (v', r) <- f v
                                             ; return (write_prep v', r)})

