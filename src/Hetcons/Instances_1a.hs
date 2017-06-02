{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Hetcons.Instances_1a () where

import Hetcons.Quorums (verify_quorums)
import Hetcons.Signed_Message (
      Recursive
       ,non_recursive
    , Recursive_1a(Recursive_1a)
       ,recursive_1a_non_recursive
       ,recursive_1a_filled_in
    , Parsable
    )

import Hetcons_Types (Proposal_1a, proposal_1a_observers)
import Data.Hashable (Hashable, hashWithSalt)


instance Hashable Recursive_1a where
  hashWithSalt s x = hashWithSalt s ((non_recursive x) :: Proposal_1a)

instance Recursive Proposal_1a Recursive_1a where
  non_recursive = recursive_1a_non_recursive

-- TODO: For a 1A object, we should check it's Observers field, and verify that it exists, and populate the quorums field. This involves verifying that the observer graph is legit.

-- | For a 1a object, we verify observer graph, and fill in quorums
instance {-# OVERLAPPING #-} Parsable Recursive_1a where
  parse payload =
    do { non_recursive <- parse payload
       ; filled_in <- verify_quorums non_recursive
       ; return Recursive_1a {
              recursive_1a_non_recursive = non_recursive
       ,recursive_1a_filled_in = non_recursive {proposal_1a_observers = Just filled_in}}}

