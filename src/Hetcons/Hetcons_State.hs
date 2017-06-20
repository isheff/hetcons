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

import Hetcons.Contains_Value (extract_1a, Contains_1a, extract_observer_quorums)
import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Signed_Message (Verified
                              ,Recursive_1b
                              ,Recursive_2b
                              ,recursive_1a_filled_in
                              ,recursive_1b_proposal
                              ,original
                              ,non_recursive)
import Hetcons.Value (garbage_collect, conflicts)

import Hetcons_Consts ()
import Hetcons_Types  (proposal_1a_observers)

import Control.Concurrent.STM (STM
                                ,atomically
                              ,TVar
                                ,writeTVar
                                ,modifyTVar'
                                ,readTVar
                                ,newTVar
                                ,readTVarIO
                              )
import Data.Foldable (toList, any)
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashSet (HashSet
                       ,intersection
                       ,fromList
                       ,empty
                       ,singleton)
import Data.Hashable (Hashable)

import Prelude ((.), (==), Bool, Foldable, IO, ($), return, Eq, id)

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

type Participant_State_Var = TVar Participant_State
type Observer_State_Var = TVar Observer_State

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
new_State :: (Foldable t, Hashable a, Eq a) => (t a) -> IO (TVar (HashSet a))
new_State = atomically . newTVar . fromList . toList

-- | a strict superset of :: IO Participant_State_Var
start_State :: (Hashable a, Eq a, Hetcons_State (HashSet a)) => IO (TVar (HashSet a))
start_State = new_State []

-- | a strict superset of :: Participant_State_Var -> IO Participant_State
read :: (TVar a) -> IO a
read = readTVarIO



-- strict superset of :: Participant_State_Var -> (Participant_State -> Participant_State) -> IO ()
modify :: (Hetcons_State a) => (TVar a) -> (a -> a) -> IO ()
modify s f = atomically $ modifyTVar' s $ write_prep . f

-- strict superset of ::  Participant_State_Var -> (Participant_State -> (Participant_State, a)) -> IO a
modify_and_read :: (Hetcons_State a) => (TVar a) -> (a -> (a, b)) -> IO b
modify_and_read s f = atomically(do {v <- readTVar s
                                    ;let (v', r) = f v
                                    ;writeTVar s $ write_prep v'
                                    ;return r})

