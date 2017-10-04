{-# LANGUAGE TemplateHaskell #-}
module Main where

import Hetcons.Demo.Participant
import Hetcons.Demo.Observer
import Hetcons.Receive_Message (flags_verbosity)

import Control.Concurrent (threadDelay)
import HFlags (initHFlags)



main :: IO ()
main = do { args <- $initHFlags "Heterogeneous Consensus Demo Observer 0.1.0.0"
          ; putStrLn "Demo Observer will run for 10 minutes\n"
          ; run_demo_observer
          ; threadDelay (1000*1000*60*10)
          }


