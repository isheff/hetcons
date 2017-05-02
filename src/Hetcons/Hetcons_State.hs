module Hetcons.Hetcons_State
    ( Hetcons_State
    ) where

import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Signed_Message (Verified
                              ,Recursive_1b)
import Hetcons.Value (garbage_collect)

import Hetcons_Consts ()
import Hetcons_Types  ()

import Control.Concurrent.STM (STM
                                ,atomically
                              ,TVar
                                ,writeTVar
                                ,modifyTVar'
                                ,readTVar
                                ,newTVar
                                ,readTVarIO
                              )
import Data.Foldable (toList)
import Data.HashSet (HashSet
                       ,intersection
                       ,fromList
                       ,singleton)


-- | literally the set of 1b messages received or sent thus far (or at least those which have been verified)
type Hetcons_State = TVar (HashSet (Verified Recursive_1b))

new_Hetcons_State :: (Foldable t) => (t (Verified Recursive_1b)) -> IO Hetcons_State
new_Hetcons_State = atomically . newTVar . fromList . toList

start_Hetcons_State :: IO Hetcons_State
start_Hetcons_State = new_Hetcons_State []


modify :: Hetcons_State -> ((HashSet (Verified Recursive_1b)) -> (HashSet (Verified Recursive_1b))) -> IO ()
modify s f = atomically $ modifyTVar' s $ garbage_collect . f

read :: Hetcons_State -> IO (HashSet (Verified Recursive_1b))
read = readTVarIO

modify_and_read ::  Hetcons_State -> ((HashSet (Verified Recursive_1b)) -> ((HashSet (Verified Recursive_1b)), a)) -> IO a
modify_and_read s f = atomically(do {v <- readTVar s
                                    ;let (v', r) = f v
                                    ;writeTVar s $ garbage_collect v'
                                    ;return r})
