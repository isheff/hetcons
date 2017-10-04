{-# LANGUAGE TemplateHaskell #-}
module Main where

import Hetcons.Demo.Participant
import Hetcons.Demo.Observer
import Hetcons.Demo.Client (flags_observers, run_demo_client)
import Hetcons.Receive_Message (flags_verbosity)

import Control.Concurrent (threadDelay)
import HFlags (initHFlags)




main :: IO ()
main = do { args <- $initHFlags "Heterogeneous Consensus Demo Client 0.1.0.0"
          ; run_demo_client
          }


