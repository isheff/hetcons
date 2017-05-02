{-# LANGUAGE OverloadedStrings #-}

module Hetcons.Quorums
    ( verify_quorums
    ) where

import Hetcons.Hetcons_Exception (Hetcons_Exception(Hetcons_Exception_Invalid_Proposal_1a))

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
                      )

import Data.Foldable (toList, length, foldr)
import qualified Data.HashSet as HashSet (map, filter)
import Data.HashMap.Lazy (HashMap, mapWithKey, filterWithKey)
import Data.HashSet (HashSet
                       ,member
                       ,intersection
                       ,fromList
                       ,toMap
                       ,singleton
                       ,empty
                       )
import Data.List (head)

verify_quorums :: Proposal_1a -> Either Hetcons_Exception Observers
verify_quorums x@(Proposal_1a { proposal_1a_observers = Nothing }) =
  Left $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
            invalid_Proposal_1a_offending_proposal = x
           ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects."}
verify_quorums x@(Proposal_1a { proposal_1a_observers = Just (Observers {observers_observer_quorums = Nothing})}) =
  Left $ Hetcons_Exception_Invalid_Proposal_1a default_Invalid_Proposal_1a {
            invalid_Proposal_1a_offending_proposal = x
           ,invalid_Proposal_1a_explanation = Just "At this time, we require all proposals to carry Observers objects featuring quorums."}
verify_quorums (Proposal_1a { proposal_1a_observers = Just x }) = Right x
-- TODO: accept observer graphs, and calculate the appropriate quorums here



