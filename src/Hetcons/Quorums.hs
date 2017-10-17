{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A Module for dealing with Quorums.
--   Specifically, verifying that an Observer Graph is viable, and calculating Observers' quorums from it.
module Hetcons.Quorums
  ( Monad_Verify_Quorums
     ,verify_quorums
     ,verify_quorums'
  ) where

import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_Invalid_Proposal_1a
                       ,Hetcons_Exception_Impossible_Observer_Graph) )

import Charlotte_Types
    ( Invalid_Proposal_1a(invalid_Proposal_1a_explanation
                         ,invalid_Proposal_1a_offending_proposal)
     ,Impossible_Observer_Graph(impossible_Observer_Graph_explanation
                               ,impossible_Observer_Graph_offending_observer_graph)
     ,Proposal_1a(Proposal_1a, proposal_1a_observers)
     ,Participant_ID
     ,Observers (Observers)
        ,observers_observer_graph
        ,observers_observer_quorums
     ,Observer_Trust_Constraint(Observer_Trust_Constraint
                               ,observer_Trust_Constraint_live
                               ,observer_Trust_Constraint_safe
                               ,observer_Trust_Constraint_observer_2
                               ,observer_Trust_Constraint_observer_1)
     ,default_Invalid_Proposal_1a
     ,default_Impossible_Observer_Graph )

import Control.Monad.Except ( MonadError(throwError) )
import Data.Foldable ( Foldable(toList, length) )
import qualified Data.HashMap.Lazy as HashMap ( fromList, (!) )
import qualified Data.HashSet as HashSet ( map, filter )
import Data.HashSet
    ( HashSet, unions, union, intersection, fromList, empty )
import qualified Data.Vector as Vector ( fromList )
import Data.Vector ( Vector, (!) )

-- | Implements the Floyd-Warshall algorithm, given a join and meet over edge weights and a 2 dimensional matrix of edge weights
--
--   returns a matrix (Vector of Vectors) of weights
floyd_warshall :: (Foldable b, Foldable c) => (a -> a -> a) -> (a -> a -> a) -> (b (c a)) -> (Vector (Vector a))
floyd_warshall a_join a_meet matrix =
  let fw (-1) i j = (v_matrix!i)!j
      fw k i j = a_meet (fw_r (k-1) i j) (a_join (fw_r (k-1) i k) (fw_r (k-1) k j))
      fw_r k i j = (((v!k)!i)!j)
      v = Vector.fromList [(Vector.fromList [(Vector.fromList [(fw k i j) | j <- [0..n]])  | i <- [0..n]])  | k <- [0..n]]
      n = (length matrix) - 1
      v_matrix = Vector.fromList $ map (Vector.fromList . toList) $ toList matrix
   in v!n

-- | We want to Memoize verify_quorums, so this class of Monads defines those Monads in which verify_quorums is memoized.
--   Notably, Hetcons_Transaction (defined in Receive_Message) is an instance.
class (MonadError Hetcons_Exception m) => Monad_Verify_Quorums m where
  -- | Verify Quorums takes in a Proposal_1a, and calculates the Observer Quorums from its observer Graph.
  --   Then, it checks if those are viable given the trust assumptions of the observer graph,
  --    and returns a new version of the Observers in the 1A, but with the Quorums populated.
  --   If the 1A already had quorums filled in, but no graph, we just leave those untouched, and don't do anything.
  --   If there are no quorums or observer graph, we throw an exception.
  verify_quorums :: Proposal_1a -> m Observers



-- | This is the non-memoized version of `verify_quorums`, and should really only be used when implementing memoization.
--   Verify Quorums takes in a Proposal_1a, and calculates the Observer Quorums from its observer Graph (`graph_to_quorums`).
--   Then, it checks if those are viable given the trust assumptions of the observer graph,
--    and returns a new version of the Observers in the 1A, but with the Quorums populated.
--   If the 1A already had quorums filled in, but no graph, we just leave those untouched, and don't do anything.
--   If there are no quorums or observer graph, we throw an exception.
verify_quorums' :: (MonadError Hetcons_Exception m) => Proposal_1a -> m Observers
-- | If there is no Observers object at all, throw an error
verify_quorums' x@(Proposal_1a { proposal_1a_observers = Nothing }) =
  throwError $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
                 invalid_Proposal_1a_offending_proposal = x
                ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects."}
-- | If there is an observer graph, run `graph_to_quorums` to calculate quorums
verify_quorums' x@(Proposal_1a { proposal_1a_observers = Just (Observers {observers_observer_graph = Just _})}) = graph_to_quorums x
-- | If there is no observer graph, and no explicitly listed quorums, throw an error.
verify_quorums' x@(Proposal_1a { proposal_1a_observers = Just (Observers {observers_observer_quorums = Nothing})}) =
  throwError $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
                 invalid_Proposal_1a_offending_proposal = x
                ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects featuring quorums or an Observer Graph."}
-- | If there is no observer graph, and there are explicitly listed quorums, just leave those be.
verify_quorums' (Proposal_1a { proposal_1a_observers = Just x }) = return x


-- | To assist in calculating Quorums from a graph, it's helpful to define a type representing an edge in that graph.
--   This is simply a set of (Safe Set, Live Set) pairs.
type Observer_Graph_Edge = HashSet (HashSet Participant_ID, HashSet Participant_ID)

-- | Remove all (safe, live) pairs from an edge which are just supersets of those found in some other pair in that edge.
--   These represent redundant information under upward closure.
reduce :: Observer_Graph_Edge -> Observer_Graph_Edge
reduce e = HashSet.filter (\(safe,live) -> (all (\(s,l) -> (((s,l) == (safe,live)) || (s /= intersection safe s) || (l /= intersection live l))) e)) e

-- | The union of two edges: all the pairs found in either (reduced, for efficiency).
e_union :: Observer_Graph_Edge -> Observer_Graph_Edge -> Observer_Graph_Edge
e_union x y = reduce $ union x y

-- | the intersection of two edges: If we fleshed out the edges with upward closure, this would just be "all pairs found in both," but we're reducing edges, so:
--   given edges E1 and E2, for all (s1,l1) in E1 ad (s2,l2) in E2, we have (s1 union s2, l1 union l2).
--   Then we reduce, for efficiency.
e_intersection :: Observer_Graph_Edge -> Observer_Graph_Edge -> Observer_Graph_Edge
e_intersection x y = reduce $ fromList [(union sx sy, union lx ly) | (sx, lx) <- toList x,  (sy, ly) <- toList y]

-- | Given a 1A featuring an observers object with an observer graph, returns a new Observers object with the quorums filled in to match the graph, or
--   throws an error if the graph isn't viable.
graph_to_quorums :: (MonadError Hetcons_Exception m) => Proposal_1a -> m Observers
graph_to_quorums x@(Proposal_1a { proposal_1a_observers = Just x_observers@(Observers {observers_observer_graph = Just constraints})}) =
  let observers = toList $ union (HashSet.map observer_Trust_Constraint_observer_1 constraints) (HashSet.map observer_Trust_Constraint_observer_2 constraints)
      participants = unions $ toList $ union (HashSet.map observer_Trust_Constraint_safe constraints) (HashSet.map observer_Trust_Constraint_live constraints)
      constraints_for x y = HashSet.filter (\c -> (x == observer_Trust_Constraint_observer_1 c && y == observer_Trust_Constraint_observer_2 c) ||
                                                  (x == observer_Trust_Constraint_observer_2 c && y == observer_Trust_Constraint_observer_1 c) ) constraints
      expanded_for x y = reduce (HashSet.map
              (\(Observer_Trust_Constraint{observer_Trust_Constraint_safe = c_safe, observer_Trust_Constraint_live = c_live}) ->
                (c_safe,
                 c_live))
                -- TODO: I'm still concerned over whether we need a constraint about safe but non-live nodes.
                -- You see, technically, a    safe live   node is normal,
                --                     , an unsafe live   node is byzantine,
                --                     , a    safe unlive node is crashed,
                --                     , an unsafe unlive node is what? Really just a shorthand for this can crash or byzantine I guess, by the upward closure rule.
              (constraints_for x y)
          )
      matrix = [[expanded_for x y | y <- observers] | x <- observers]
      cog = floyd_warshall e_intersection e_union matrix
      quorums = HashMap.fromList [(pid, ((unions $ map  (HashSet.map snd)  $ toList row):: (HashSet (HashSet Participant_ID))))
                                 | (pid,row) <- (zip observers (toList matrix))]
      valid_quorums = all (\(x,row) -> (all (\(y, sls) -> (all (\(safe, live) -> (
                      all (\qx -> (all (\qy -> (
                        -- (qx /= intersection qx live) || -- In the formal math, we have the check that both quorums be live, however, given the upward closure
                        -- (qy /= intersection qy live) || -- requirement, we know that if the quorums aren't live, there's an equivalent case in which they are live.
                        (empty /= intersection safe (intersection qx qy)) -- this is where the crucial requirement is
                      )) $ quorums HashMap.! y)) $ quorums HashMap.! x))
                      sls)) $ zip observers $ toList row)) $ zip observers $ toList matrix
   in if valid_quorums
         then return (x_observers {observers_observer_quorums = Just quorums})
         else throwError $ Hetcons_Exception_Impossible_Observer_Graph (default_Impossible_Observer_Graph {
                             impossible_Observer_Graph_offending_observer_graph = constraints
                            ,impossible_Observer_Graph_explanation = Just $ "These constraints result in a set of quorums that doesn't meet the quorum requirement."})


-- | Given a 1A not featuring a Graph, just throws an error.
graph_to_quorums x = throwError $
         Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
            invalid_Proposal_1a_offending_proposal = x
           ,invalid_Proposal_1a_explanation = Just "Unable to calculate quorums from non-existent graph."}



