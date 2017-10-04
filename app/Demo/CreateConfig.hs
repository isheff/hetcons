{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Hetcons.Demo.Client(flags_observers)
import Hetcons.Receive_Message (flags_verbosity)

import Hetcons_Types
    ( Participant_ID(participant_ID_crypto_id, participant_ID_address)
                    ,default_Participant_ID
     ,Slot_Value(slot_Value_slot, slot_Value_value_payload)
           ,default_Slot_Value
     ,Observers(observers_observer_graph, observers_observer_quorums)
               ,default_Observers
               ,encode_Observers
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
     )

import qualified Data.ByteString.Lazy as ByteString
    ( writeFile, readFile )
import Data.HashSet ( insert, fromList, empty )
import Data.Text.Lazy ( pack )
import HFlags (initHFlags)
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Empty ( EmptyTransport(EmptyTransport) )

sample_id cert port =
  default_Participant_ID  {
    participant_ID_address =
      default_Address  {
        address_host_address =
          default_Host_Address  {
            host_Address_dns_name = Just $ pack "localhost"}
       ,address_port_number = port}
   ,participant_ID_crypto_id =
      default_Crypto_ID {
        crypto_ID_public_crypto_key =
          Just (default_Public_Crypto_Key {
                  public_Crypto_Key_public_crypto_key_x509 = Just cert})}}

main :: IO ()
main = do { args <- $initHFlags "Heterogeneous Consensus Demo create config 0.1.0.0"
          ; cert <- ByteString.readFile "test/cert.pem"
          ; certs' <- mapM (\i -> ByteString.readFile $ "test/cert" ++ (show i) ++ ".pem") [1..6]
          ; let (ids :: [Participant_ID]) = map (uncurry sample_id) (zip (cert:certs') [85020..85026])
          ; let observers = default_Observers {
                  observers_observer_graph = Just $ fromList $ [
                          default_Observer_Trust_Constraint  {
                            observer_Trust_Constraint_observer_1 = ids!!0
                           ,observer_Trust_Constraint_observer_2 = ids!!6
                           ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2, ids!!3        ]
                           ,observer_Trust_Constraint_live = fromList $ [ids!!1, ids!!2, ids!!3        ]}
                         ,default_Observer_Trust_Constraint  {
                            observer_Trust_Constraint_observer_1 = ids!!0
                           ,observer_Trust_Constraint_observer_2 = ids!!6
                           ,observer_Trust_Constraint_safe = fromList $ [ids!!1, ids!!2,         ids!!4]
                           ,observer_Trust_Constraint_live = fromList $ [ids!!1, ids!!2,         ids!!4]}
                         ,default_Observer_Trust_Constraint  {
                            observer_Trust_Constraint_observer_1 = ids!!0
                           ,observer_Trust_Constraint_observer_2 = ids!!6
                           ,observer_Trust_Constraint_safe = fromList $ [ids!!1,         ids!!3, ids!!4]
                           ,observer_Trust_Constraint_live = fromList $ [ids!!1,         ids!!3, ids!!4]}
                         ,default_Observer_Trust_Constraint  {
                            observer_Trust_Constraint_observer_1 = ids!!0
                           ,observer_Trust_Constraint_observer_2 = ids!!6
                           ,observer_Trust_Constraint_safe = fromList $ [        ids!!2, ids!!3, ids!!4]
                           ,observer_Trust_Constraint_live = fromList $ [        ids!!2, ids!!3, ids!!4]}]}
          ; ByteString.writeFile flags_observers (encode_Observers (BinaryProtocol EmptyTransport) observers)
          }
