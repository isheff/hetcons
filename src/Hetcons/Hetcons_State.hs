module Hetcons.Hetcons_State
    ( Hetcons_State_Var
    , Hetcons_State
    , conflicting_state
    , new_Hetcons_State
    , start_Hetcons_State
    , modify
    , read
    , modify_and_read
    ) where

import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Signed_Message (Verified
                              ,Recursive_1b)
import Hetcons.Value (garbage_collect, conflicts)

import Hetcons_Consts ()
import Hetcons_Types  (proposal_1a_observers
                      ,recursive_1a_filled_in
                      ,recursive_1b_proposal)

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
                       ,singleton)


-- | literally the set of 1b messages received or sent thus far (or at least those which have been verified)
type Hetcons_State = HashSet (Verified Recursive_1b)
type Hetcons_State_Var = TVar Hetcons_State

-- | Are there any conflicting proposals in this state?
-- | Bear in mind that two proposals with different COGs NEVER CONFLICT.
-- | We make no guarantees about different COGs.
-- | This is not implemented in a computationally efficient manner.
conflicting_state :: Hetcons_State -> Bool
conflicting_state s =
  any conflicts  -- Subsets of the proposals which have the same COG, for each COG in the Hetcons_State
      (HashSet.map (\x -> (HashSet.filter ((x ==) . proposal_1a_observers . recursive_1a_filled_in . recursive_1b_proposal) s)) -- 1bs per COG
                   (HashSet.map (proposal_1a_observers . recursive_1a_filled_in . recursive_1b_proposal) s)) -- all the COGs

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
