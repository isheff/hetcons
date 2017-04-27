module Hetcons.Contains_Value
    ( Contains_Value
        ,extract_value
    ) where

import Hetcons.Signed_Message (Recursive_1b
                                 ,recursive_1b_proposal
                              ,Recursive_1a (Recursive_1a )
                                 ,recursive_1a_filled_in
                              ,Recursive_2a (Recursive_2a )
                              ,Recursive_2b (Recursive_2b )
                              ,Recursive_Proof_of_Consensus (Recursive_Proof_of_Consensus)
                              ,Parsable
                              ,Verified
                                 ,original
                              )

import Hetcons_Consts ()
import Hetcons_Types  (Value
                      ,Proposal_1a
                         ,proposal_1a_value
                      )

import Data.Foldable (toList)
import Data.List (head)


class Contains_Value a where
  extract_value :: a -> Value

instance (Parsable a, Contains_Value a) => Contains_Value (Verified a) where
  extract_value = extract_value . original
instance Contains_Value Value where
  extract_value = id
instance Contains_Value Proposal_1a where
  extract_value = proposal_1a_value
instance Contains_Value Recursive_1a where
  extract_value = extract_value . recursive_1a_filled_in
instance Contains_Value Recursive_1b where
  extract_value = extract_value . recursive_1b_proposal
instance Contains_Value Recursive_2a where
  extract_value (Recursive_2a x) = extract_value $ head $ toList x
instance Contains_Value Recursive_2b where
  extract_value (Recursive_2b x) = extract_value $ head $ toList x
instance Contains_Value Recursive_Proof_of_Consensus where
  extract_value (Recursive_Proof_of_Consensus x) = extract_value $ head $ toList x




