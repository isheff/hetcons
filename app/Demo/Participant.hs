{-# LANGUAGE TemplateHaskell #-}
module Main where

import Hetcons.Demo.Participant

import Control.Concurrent (threadDelay)
import HFlags (initHFlags)



main :: IO ()
main = do { args <- $initHFlags "Heterogeneous Consensus Demo Participant 0.1.0.0"
          ; putStrLn "Demo Participant will run for 10 minutes\n"
          ; run_demo_participant
          ; threadDelay (1000*1000*60*10)
          }

