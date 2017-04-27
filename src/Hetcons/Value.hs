module Hetcons.Value
    ( conflicts
    , garbage_collect
    ) where

import Hetcons.Contains_Value (Contains_Value
                                 ,extract_value)
import Hetcons.Hetcons_Exception (Hetcons_Exception())
import Hetcons.Signed_Message (Recursive
                                 ,non_recursive
                              ,Recursive_1b
                                 ,recursive_1b_proposal
                              ,Recursive_2a (Recursive_2a )
                              ,Recursive_2b (Recursive_2b )
                              ,Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
                              ,Parsable
                              ,Verified
                                 ,original
                              )

import Hetcons_Consts ()
import Hetcons_Types  (Value
                         ,value_value_payload
                         ,value_slot
                         ,default_Value
                      ,Proposal_1a
                         ,proposal_1a_value
                      )

import Data.Foldable (toList, length)
import qualified Data.HashSet as HashSet (map)
import Data.HashSet (HashSet
                       ,member
                       ,intersection
                       ,fromList
                       )
import Data.List (head)




-- | Does this set of entities contain conflicitng values?
-- | In this case, do any two of them have the same value_slot?
conflicts :: (Contains_Value a) => (HashSet a) -> Bool
conflicts s = (length s) == (length (HashSet.map (value_slot . extract_value) s))

-- | Specifically, if we're storing a set of messages received thus far, we should kick out ones with, say, useless timestamps.
garbage_collect :: (Contains_Value a, Parsable a) => (HashSet (Verified a)) -> (HashSet (Verified a))
garbage_collect = id



