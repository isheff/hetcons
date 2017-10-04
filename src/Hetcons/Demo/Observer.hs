{-# LANGUAGE TemplateHaskell #-}

-- | A demo Participant Server, which runs the Consensus algorithm, ultimately "accepting" and sending a 2B to Observer Servers
module Hetcons.Demo.Observer (run_demo_observer) where

import Hetcons.Demo.Participant (flags_port, flags_cert, flags_key, flags_datatype, run_demo_participant, Consensus_Datatype(Consensus_Datatype_Slot_Value))

import Hetcons.Observer (Observer, new_observer, observer_server, on_consensus)
import Hetcons_Types
    ( Slot_Value(slot_Value_slot, slot_Value_value_payload)
     ,Public_Crypto_Key(public_Crypto_Key_public_crypto_key_x509)
                       ,default_Public_Crypto_Key
     ,Crypto_ID(crypto_ID_public_crypto_key)
               ,default_Crypto_ID
     )

import Control.Concurrent (ThreadId)
import qualified Data.ByteString.Lazy as ByteString (readFile)
import HFlags (defineFlag, defineEQFlag)

run_demo_observer :: IO ThreadId
run_demo_observer = do { cert <- ByteString.readFile flags_cert
                       ; private <- ByteString.readFile flags_key
                       ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                                            Just (default_Public_Crypto_Key {
                                              public_Crypto_Key_public_crypto_key_x509 = Just cert})}
                       ; case flags_datatype of
                           Consensus_Datatype_Slot_Value -> do { observer <- (new_observer crypto_id private on_consensus ) :: (IO (Observer Slot_Value))
                                                                  ; observer_server observer flags_port}}


