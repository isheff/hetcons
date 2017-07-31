{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hetcons.Quorums
  ( Monad_Verify_Quorums
     ,verify_quorums
     ,verify_quorums'
  ) where

import Hetcons.Hetcons_Exception
    ( Hetcons_Exception(Hetcons_Exception_Invalid_Proposal_1a
                       ,Hetcons_Exception_Impossible_Observer_Graph) )

import Hetcons_Types
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
                               ,observer_Trust_Constraint_live, observer_Trust_Constraint_safe
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

floyd_warshall :: (Foldable b, Foldable c) => (a -> a -> a) -> (a -> a -> a) -> (b (c a)) -> (Vector (Vector a))
floyd_warshall a_join a_meet matrix =
  let fw (-1) i j = (v_matrix!i)!j
      fw k i j = a_meet (fw_r (k-1) i j) (a_join (fw_r (k-1) i k) (fw_r (k-1) k j))
      fw_r k i j = (((v!k)!i)!j)
      v = Vector.fromList [(Vector.fromList [(Vector.fromList [(fw k i j) | j <- [0..n]])  | i <- [0..n]])  | k <- [0..n]]
      n = (length matrix) - 1
      v_matrix = Vector.fromList $ map (Vector.fromList . toList) $ toList matrix
   in v!n




class (MonadError Hetcons_Exception m) => Monad_Verify_Quorums m where
  verify_quorums :: Proposal_1a -> m Observers

-- TODO: this instance is used in testing only, so we should move it over to tests
instance {-# OVERLAPPABLE #-} (MonadError Hetcons_Exception m) => Monad_Verify_Quorums m where
  verify_quorums = verify_quorums'


-- | a strict generalization of :: Proposal_1a -> Either Hetcons_Exception Observers
verify_quorums' :: (MonadError Hetcons_Exception m) => Proposal_1a -> m Observers
verify_quorums' x@(Proposal_1a { proposal_1a_observers = Nothing }) =
  throwError $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
                 invalid_Proposal_1a_offending_proposal = x
                ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects."}
verify_quorums' x@(Proposal_1a { proposal_1a_observers = Just (Observers {observers_observer_graph = Just _})}) = graph_to_quorums x
verify_quorums' x@(Proposal_1a { proposal_1a_observers = Just (Observers {observers_observer_quorums = Nothing})}) =
  throwError $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
                 invalid_Proposal_1a_offending_proposal = x
                ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects featuring quorums."}
verify_quorums' (Proposal_1a { proposal_1a_observers = Just x }) = return x


type Observer_Graph_Edge = HashSet (HashSet Participant_ID, HashSet Participant_ID)
reduce :: Observer_Graph_Edge -> Observer_Graph_Edge
reduce e = HashSet.filter (\(safe,live) -> (all (\(s,l) -> (((s,l) == (safe,live)) || (s /= intersection safe s) || (l /= intersection live l))) e)) e

e_union :: Observer_Graph_Edge -> Observer_Graph_Edge -> Observer_Graph_Edge
e_union x y = reduce $ union x y

e_intersection :: Observer_Graph_Edge -> Observer_Graph_Edge -> Observer_Graph_Edge
e_intersection x y = reduce $ fromList [(union sx sy, union lx ly) | (sx, lx) <- toList x,  (sy, ly) <- toList y]

-- TODO: this is very slow, mostly due to the fact that we manually calculate powersets. We can probably do this much more efficiently.
graph_to_quorums :: (MonadError Hetcons_Exception m) => Proposal_1a -> m Observers
graph_to_quorums x@(Proposal_1a { proposal_1a_observers = Just x_observers@(Observers {observers_observer_graph = Just constraints})}) =
  let observers = toList $ union (HashSet.map observer_Trust_Constraint_observer_1 constraints) (HashSet.map observer_Trust_Constraint_observer_2 constraints)
      participants = unions $ toList $ union (HashSet.map observer_Trust_Constraint_safe constraints) (HashSet.map observer_Trust_Constraint_live constraints)
      -- power_set = fromList $ map fromList $ filterM (const [True, False]) (toList participants)
      -- power_constraints = fromList [(s,l) | s <- toList power_set, l <- toList power_set]
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
                        -- (qx /= intersection qx live) || -- this is where the crucial requirement is
                        -- (qy /= intersection qy live) || --
                        (empty /= intersection safe (intersection qx qy))
                      )) $ quorums HashMap.! y)) $ quorums HashMap.! x))
                      sls)) $ zip observers $ toList row)) $ zip observers $ toList matrix
   in if valid_quorums
         then return (x_observers {observers_observer_quorums = Just quorums})
         else throwError $ Hetcons_Exception_Impossible_Observer_Graph (default_Impossible_Observer_Graph {
                             impossible_Observer_Graph_offending_observer_graph = constraints
                            ,impossible_Observer_Graph_explanation = Just $ "These constraints result in a set of quorums that doesn't meet the quorum requirement."})


graph_to_quorums x = throwError $
         Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
            invalid_Proposal_1a_offending_proposal = x
           ,invalid_Proposal_1a_explanation = Just "Unable to calculate quorums from non-existent graph."}



