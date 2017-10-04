{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A demo Participant Server, which runs the Consensus algorithm, ultimately "accepting" and sending a 2B to Observer Servers
module Hetcons.Demo.Client (flags_observers, run_demo_client) where

import Hetcons.Demo.Participant (flags_port, flags_cert, flags_key, flags_datatype, run_demo_participant, Consensus_Datatype(Consensus_Datatype_Slot_Value))
import Hetcons.Hetcons_Exception (Hetcons_Exception)
import Hetcons.Parsable (Parsable)
import Hetcons.Participant (Participant, new_participant, participant_server, current_nanoseconds)
import Hetcons.Quorums (Monad_Verify_Quorums, verify_quorums', verify_quorums)
import Hetcons.Send_Message_IO (Address_Book, default_Address_Book, send_Message_IO, domain_name)
import Hetcons.Signed_Message (encode, sign, verify, verify', Verified, Recursive_1a, Monad_Verify)

import Hetcons_Consts ( sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR )
import Hetcons_Types
    ( Participant_ID(participant_ID_crypto_id, participant_ID_address)
                    ,default_Participant_ID
     ,Slot_Value(slot_Value_slot, slot_Value_value_payload)
           ,default_Slot_Value
     ,Observers(observers_observer_graph, observers_observer_quorums)
               ,default_Observers
               ,decode_Observers
     ,Proposal_1a(proposal_1a_observers, proposal_1a_timestamp
                 ,proposal_1a_value)
                 ,default_Proposal_1a
     ,Public_Crypto_Key(public_Crypto_Key_public_crypto_key_x509)
                       ,default_Public_Crypto_Key
     ,Crypto_ID(crypto_ID_public_crypto_key)
               ,default_Crypto_ID
     ,Signed_Message
     ,Phase_1b(phase_1b_proposal)
              ,default_Phase_1b
     ,Phase_2b(phase_2b_phase_1bs)
              ,default_Phase_2b
     ,Host_Address(host_Address_dns_name)
                  ,default_Host_Address
     ,Timestamp
     ,Address(address_port_number, address_host_address)
             ,default_Address
     ,Observer_Trust_Constraint(observer_Trust_Constraint_live
                               ,observer_Trust_Constraint_safe
                               ,observer_Trust_Constraint_observer_2
                               ,observer_Trust_Constraint_observer_1)
                               ,default_Observer_Trust_Constraint
     ,Crypto_ID_Hash
     ,IPv4_Address
     ,IPv6_Address
     ,Hash
     ,Public_Crypto_Key_Type_Descriptor
     )

import qualified Data.ByteString.Lazy as ByteString
    ( writeFile, readFile, ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString (pack)
import Control.Concurrent (ThreadId)
import Control.Monad (forever)
import Control.Monad.Except ( MonadError, throwError )
import Crypto.Random ( getSystemDRG, DRG, withDRG )
import Data.Int (Int64)
import Data.Text (strip, pack, unpack)
import HFlags (defineFlag)
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )



defineFlag "o:observers" "test/observers.conf"   "file name of a JSON-encoded observers object to be used in client proposals"

instance {-# OVERLAPPABLE #-} (MonadError Hetcons_Exception m) => Monad_Verify_Quorums m where
  verify_quorums = verify_quorums'

instance {-# OVERLAPPABLE #-} (Monad_Verify_Quorums m
                              ,MonadError Hetcons_Exception m, Parsable (m a))
                               => Monad_Verify a m where
  verify = verify'

repl :: ByteString.ByteString -> Crypto_ID -> Observers -> Address_Book -> IO ()
repl private crypto_id observers address_book =
  case flags_datatype of
    Consensus_Datatype_Slot_Value -> forever (do { putStrLn "Enter a proposal in the form (SLOT :: Int) : PAYLOAD\n"
                                                 ; now <- current_nanoseconds
                                                 ; gen <- getSystemDRG
                                                 ; line <- getLine
                                                 ; let (slot :: Int64) = read $ unpack $ strip $ pack $ takeWhile (/=':') line
                                                 ; let value = ByteString.pack $ tail $ dropWhile (/=':') line
                                                 ; let message_1a = default_Proposal_1a {
                                                                      proposal_1a_value = encode default_Slot_Value {
                                                                                             slot_Value_value_payload = value
                                                                                            ,slot_Value_slot = slot}
                                                                     ,proposal_1a_timestamp = now
                                                                     ,proposal_1a_observers = Just observers}
                                                 ; let (Right signed_1a) = sign crypto_id private sUPPORTED_SIGNED_HASH_TYPE_DESCRIPTOR gen message_1a
                                                 ; let (Right (v1a :: (Verified (Recursive_1a Slot_Value)))) = verify signed_1a
                                                 ; putStrLn "sending ... \n"
                                                 ; send_Message_IO address_book v1a
                                                 })

run_demo_client  :: IO ()
run_demo_client = do { observers_file <- ByteString.readFile flags_observers
                     ; let observers = decode_Observers (BinaryProtocol EmptyTransport) observers_file
                     ; cert <- ByteString.readFile flags_cert
                     ; address_book <- default_Address_Book
                     ; private <- ByteString.readFile flags_key
                     ; let crypto_id = default_Crypto_ID {crypto_ID_public_crypto_key =
                                          Just (default_Public_Crypto_Key {
                                            public_Crypto_Key_public_crypto_key_x509 = Just cert})}
                     ; repl private crypto_id observers address_book
                     }
