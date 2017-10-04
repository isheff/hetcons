{-# LANGUAGE TemplateHaskell #-}

-- | A demo Participant Server, which runs the Consensus algorithm, ultimately "accepting" and sending a 2B to Observer Servers
module Hetcons.Demo.Participant (flags_port, flags_cert, flags_key, flags_datatype, run_demo_participant, Consensus_Datatype(Consensus_Datatype_Slot_Value)) where

import Hetcons.Participant (Participant, new_participant, participant_server)
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


defineFlag "p:port" (8080 :: Int)   "port number for this server to run on"
defineFlag "c:cert" "test/cert.pem" "public certificate file"
defineFlag "k:key"  "test/key.pem"  "private key file"


data Consensus_Datatype = Consensus_Datatype_Slot_Value deriving (Show, Read)
defineEQFlag "d:datatype" [| Consensus_Datatype_Slot_Value :: Consensus_Datatype |] "CONSENSUS_DATATYPE" "client datatype."


run_demo_participant :: IO ThreadId
run_demo_participant = do { cert <- ByteString.readFile flags_cert
                          ; private <- ByteString.readFile flags_key
                          ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                                               Just (default_Public_Crypto_Key {
                                                 public_Crypto_Key_public_crypto_key_x509 = Just cert})}
                          ; case flags_datatype of
                                 Consensus_Datatype_Slot_Value -> do { participant <- (new_participant crypto_id private) :: (IO (Participant Slot_Value))
                                                                     ; participant_server participant flags_port}}

