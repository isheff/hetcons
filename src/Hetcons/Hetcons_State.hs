module Hetcons.Hetcons_State
    ( Hetcons_State_Var
    , Hetcons_State
    , default_Hetcons_State
    , conflicting_state
    , new_Hetcons_State
    , start_Hetcons_State
    , modify
    , read
    , modify_and_read
    , state_by_observers
    ) where

import Hetcons.Contains_Value (extract_1a)
import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Signed_Message (Verified
                              ,Recursive_1b
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

import Prelude ((.), (==), Bool, Foldable, IO, ($), return)


-- | literally the set of 1b messages received or sent thus far (or at least those which have been verified)
type Hetcons_State = HashSet (Verified Recursive_1b)
default_Hetcons_State :: Hetcons_State
default_Hetcons_State = empty

type Hetcons_State_Var = TVar Hetcons_State

-- | Subsets of the proposals which have the same COG, for each COG in the Hetcons_State
state_by_observers :: Hetcons_State -> (HashSet (Hetcons_State))
state_by_observers s =
  (HashSet.map (\x -> (HashSet.filter ((x ==) . proposal_1a_observers . non_recursive . original . extract_1a) s)) -- 1bs per COG
               (HashSet.map (proposal_1a_observers . non_recursive . original . extract_1a) s)) -- all the COGs

-- | Are there any conflicting proposals in this state?
-- | Bear in mind that two proposals with different COGs NEVER CONFLICT.
-- | We make no guarantees about different COGs.
-- | This is not implemented in a computationally efficient manner.
conflicting_state :: Hetcons_State -> Bool
conflicting_state = (any conflicts) . state_by_observers

new_Hetcons_State :: (Foldable t) => (t (Verified Recursive_1b)) -> IO Hetcons_State_Var
new_Hetcons_State = atomically . newTVar . fromList . toList

start_Hetcons_State :: IO Hetcons_State_Var
start_Hetcons_State = new_Hetcons_State []


modify :: Hetcons_State_Var -> (Hetcons_State -> Hetcons_State) -> IO ()
modify s f = atomically $ modifyTVar' s $ garbage_collect . f

read :: Hetcons_State_Var -> IO Hetcons_State
read = readTVarIO

modify_and_read ::  Hetcons_State_Var -> (Hetcons_State -> (Hetcons_State, a)) -> IO a
modify_and_read s f = atomically(do {v <- readTVar s
                                    ;let (v', r) = f v
                                    ;writeTVar s $ garbage_collect v'
                                    ;return r})
