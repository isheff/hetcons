{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hetcons.Quorums
    ( verify_quorums
    ) where

import Hetcons.Hetcons_Exception (Hetcons_Exception(Hetcons_Exception_Invalid_Proposal_1a
                                                   ,Hetcons_Exception_Impossible_Observer_Graph))

import Hetcons_Consts ()
import Hetcons_Types  (Value
                         ,value_slot
                      ,Proposal_1a (Proposal_1a)
                         ,proposal_1a_value
                         ,proposal_1a_observers
                      ,Crypto_ID
                      ,Participant_ID (Participant_ID)
                         ,participant_ID_crypto_id
                      ,Observers (Observers)
                         ,observers_observer_graph
                         ,observers_observer_quorums
                      ,Invalid_Proposal_1a
                         ,invalid_Proposal_1a_offending_proposal
                         ,invalid_Proposal_1a_explanation
                         ,default_Invalid_Proposal_1a
                      ,Observers (Observers)
                         ,observers_observer_graph
                         ,observers_observer_quorums
                      ,Observer_Trust_Constraint(Observer_Trust_Constraint)
                         ,observer_Trust_Constraint_observer_1
                         ,observer_Trust_Constraint_observer_2
                         ,observer_Trust_Constraint_safe
                         ,observer_Trust_Constraint_live
                         ,default_Observers
                      ,Impossible_Observer_Graph
                         ,default_Impossible_Observer_Graph
                         ,impossible_Observer_Graph_offending_observer_graph
                         ,impossible_Observer_Graph_explanation
                      )

import Control.Monad (filterM)
import Control.Monad.Except (throwError)
import Data.Foldable (toList, length, foldr)
import qualified Data.HashMap.Lazy as HashMap (fromList,(!))
import Data.HashMap.Lazy (HashMap, mapWithKey, filterWithKey)
import qualified Data.HashSet as HashSet (map, filter, fromList)
import Data.HashSet (HashSet
                       ,member
                       ,intersection
                       ,union
                       ,unions
                       ,fromList
                       ,toMap
                       ,singleton
                       ,empty
                       )
import Data.List (head)
import qualified Data.Vector as Vector (fromList)
import Data.Vector (Vector, (!))


floyd_warshall :: (Foldable b, Foldable c) => (a -> a -> a) -> (a -> a -> a) -> (b (c a)) -> (Vector (Vector a))
floyd_warshall a_join a_meet matrix =
  let fw (-1) i j = (v_matrix!i)!j
      fw k i j = a_meet (fw_r (k-1) i j) (a_join (fw_r (k-1) i k) (fw_r (k-1) k j))
      fw_r k i j = (((v!k)!i)!j)
      v = Vector.fromList [(Vector.fromList [(Vector.fromList [(fw k i j) | j <- [0..n]])  | i <- [0..n]])  | k <- [0..n]]
      n = (length matrix) - 1
      v_matrix = Vector.fromList $ map (Vector.fromList . toList) $ toList matrix
   in v!n






verify_quorums :: Proposal_1a -> Either Hetcons_Exception Observers
verify_quorums x@(Proposal_1a { proposal_1a_observers = Nothing }) =
  Left $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
            invalid_Proposal_1a_offending_proposal = x
           ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects."}
verify_quorums x@(Proposal_1a { proposal_1a_observers = Just (Observers {observers_observer_graph = Just _})}) = graph_to_quorums x
verify_quorums x@(Proposal_1a { proposal_1a_observers = Just (Observers {observers_observer_quorums = Nothing})}) =
  Left $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
            invalid_Proposal_1a_offending_proposal = x
           ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects featuring quorums."}
verify_quorums (Proposal_1a { proposal_1a_observers = Just x }) = Right x


graph_to_quorums :: Proposal_1a -> Either Hetcons_Exception Observers
graph_to_quorums x@(Proposal_1a { proposal_1a_observers = Just x_observers@(Observers {observers_observer_graph = Just constraints})}) =
  let observers = toList $ union (HashSet.map observer_Trust_Constraint_observer_1 constraints) (HashSet.map observer_Trust_Constraint_observer_2 constraints)
      participants = unions $ toList $ union (HashSet.map observer_Trust_Constraint_safe constraints) (HashSet.map observer_Trust_Constraint_live constraints)
      power_set = fromList $ map fromList $ filterM (const [True, False]) (toList participants)
      power_constraints = fromList [(s,l) | s <- toList power_set, l <- toList power_set]
      constraints_for x y = HashSet.filter (\c -> (x == observer_Trust_Constraint_observer_1 c && y == observer_Trust_Constraint_observer_2 c) ||
                                                  (x == observer_Trust_Constraint_observer_2 c && y == observer_Trust_Constraint_observer_1 c) ) constraints

      expanded_for x y = HashSet.filter (\(safe, live) -> (
          any (\(Observer_Trust_Constraint{observer_Trust_Constraint_safe = c_safe, observer_Trust_Constraint_live = c_live}) ->
                (c_safe == intersection c_safe safe) &&
                (c_live == intersection c_live live) &&
                (safe == intersection safe live)) -- the set of live nodes must include the set of safe nodes.
              (constraints_for x y)
        )) power_constraints
      matrix = [[expanded_for x y | y <- observers] | x <- observers]
      cog = floyd_warshall intersection union matrix
      quorums = HashMap.fromList [(pid, ((unions $ map  (HashSet.map snd)  $ toList row):: (HashSet (HashSet Participant_ID)))) | (pid,row) <- (zip observers (toList matrix))]
      valid_quorums = all (\(x,row) -> (all (\(y, sls) -> (all (\(safe, live) -> (
                      all (\qx -> (all (\qy -> (
                        (qx /= intersection qx live) || -- this is where the crucial requirement is
                        (qy /= intersection qy live) || --
                        (empty /= intersection safe (intersection qx qy))
                      )) $ quorums HashMap.! y)) $ quorums HashMap.! x))
                      sls)) $ zip observers $ toList row)) $ zip observers $ toList matrix
   in if valid_quorums
         then Right (x_observers {observers_observer_quorums = Just quorums})
         else Left $ Hetcons_Exception_Impossible_Observer_Graph (default_Impossible_Observer_Graph {
                       impossible_Observer_Graph_offending_observer_graph = constraints
                      ,impossible_Observer_Graph_explanation = Just $ "These constraints result in a set of quorums that doesn't meet the quorum requirement."})


graph_to_quorums x = throwError $
         Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
            invalid_Proposal_1a_offending_proposal = x
           ,invalid_Proposal_1a_explanation = Just "Unable to calculate quorums from non-existent graph."}



