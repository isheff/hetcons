{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Hetcons.Contains_Value
    ( Contains_1a
     ,extract_observer_quorums )
import Hetcons.Instances_1b_2a ()
import Hetcons.Signed_Message
    ( Recursive_1b
     ,Verified
     ,Recursive_2b )
import Hetcons.Value ( garbage_collect, conflicts )

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


class Hetcons_State a where
  write_prep :: a -> a
instance Hetcons_State Participant_State where
  write_prep = garbage_collect
instance Hetcons_State Observer_State where
  write_prep = id

-- | literally the set of 1b messages received or sent thus far (or at least those which have been verified)
type Participant_State = HashSet (Verified Recursive_1b)
type Observer_State = HashSet (Verified Recursive_2b)

default_State :: (HashSet a)
default_State = empty

type Participant_State_Var = MVar Participant_State
type Observer_State_Var = MVar Observer_State

-- | Subsets of the proposals which have the same COG, for each COG in the Observer_State
-- | strict superset of :: Participant_State -> (HashSet (Participant_State))
state_by_observers :: (Contains_1a a, Hashable a, Eq a) => (HashSet a) -> (HashSet (HashSet a))
state_by_observers s =
  (HashSet.map (\x -> (HashSet.filter ((x ==) . extract_observer_quorums) s)) -- 1bs per COG
               (HashSet.map extract_observer_quorums s)) -- all the COGs

-- | Are there any conflicting proposals in this state?
-- | Bear in mind that two proposals with different COGs NEVER CONFLICT.
-- | We make no guarantees about different COGs.
-- | This is not implemented in a computationally efficient manner.
conflicting_state :: Participant_State -> Bool
conflicting_state = (any conflicts) . state_by_observers

-- | a strict superset of :: (Foldable t) => (t (Verified Recursive_1b)) -> IO Participant_State_Var
new_State :: (Foldable t, Hashable a, Eq a) => (t a) -> IO (MVar (HashSet a))
new_State = newMVar . fromList . toList

-- | a strict superset of :: IO Participant_State_Var
start_State :: (Hashable a, Eq a, Hetcons_State (HashSet a)) => IO (MVar (HashSet a))
start_State = new_State []

-- | a strict superset of :: Participant_State_Var -> IO Participant_State
read :: (MVar a) -> IO a
read = readMVar



-- strict superset of :: Participant_State_Var -> (Participant_State -> Participant_State) -> IO ()
modify :: (Hetcons_State a) => (MVar a) -> (a -> a) -> IO ()
modify s f = modifyMVar_ s $ return . write_prep . f

-- strict superset of ::  Participant_State_Var -> (Participant_State -> (Participant_State, a)) -> IO a
modify_and_read :: (Hetcons_State a) => (MVar a) -> (a -> (IO (a, b))) -> IO b
modify_and_read s f = modifyMVar s (\v -> do { (v', r) <- f v
                                             ; return (write_prep v', r)})

