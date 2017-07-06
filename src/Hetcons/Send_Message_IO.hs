{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

module Hetcons.Send_Message_IO (Address_Book, default_Address_Book, send_Message_IO, domain_name) where

import Hetcons.Contains_Value (extract_observer_quorums, extract_1bs)
import Hetcons.Hetcons_Exception  (Hetcons_Exception)
import Hetcons.Instances_1a ()
import Hetcons.Instances_1b_2a ()
import Hetcons.Instances_2b ()
import Hetcons.Instances_Proof_of_Consensus (observers_proven)
import Hetcons.Signed_Message     (Verified
                                    ,original
                                    ,signed
                                  ,Recursive_1a
                                  ,Recursive_1b
                                  ,Recursive_2a
                                  ,Recursive_2b
                                  ,Recursive_Proof_of_Consensus(Recursive_Proof_of_Consensus)
                                  ,Parsable, sign)

import Hetcons_Observer_Client (phase_2b)
import Hetcons_Participant_Client (proposal_1a, phase_1b)
import Hetcons_Types              (Proposal_1a, Phase_1b, Phase_2a, Phase_2b, Proof_of_Consensus, Signed_Message
                                  ,Participant_ID(Participant_ID)
                                    ,participant_ID_address
                                  ,Address(Address)
                                    ,address_host_address
                                    ,address_port_number
                                  ,Host_Address(Host_Address)
                                    ,host_Address_dns_name
                                    ,host_Address_ipv4_address
                                    ,host_Address_ipv6_address
                                  ,IPv4_Address(IPv4_Address)
                                    ,iPv4_Address_byte_0
                                    ,iPv4_Address_byte_1
                                    ,iPv4_Address_byte_2
                                    ,iPv4_Address_byte_3
                                  ,IPv6_Address(IPv6_Address)
                                    ,iPv6_Address_byte_0
                                    ,iPv6_Address_byte_1
                                    ,iPv6_Address_byte_2
                                    ,iPv6_Address_byte_3
                                    ,iPv6_Address_byte_4
                                    ,iPv6_Address_byte_5
                                    ,iPv6_Address_byte_6
                                    ,iPv6_Address_byte_7
                                    ,iPv6_Address_byte_8
                                    ,iPv6_Address_byte_9
                                    ,iPv6_Address_byte_10
                                    ,iPv6_Address_byte_11
                                    ,iPv6_Address_byte_12
                                    ,iPv6_Address_byte_13
                                    ,iPv6_Address_byte_14
                                    ,iPv6_Address_byte_15
                                  )

import qualified Control.Concurrent.Map as Concurrent_Map (Map, empty, lookup)
import Control.Concurrent.Map     (insertIfAbsent)
import Control.Concurrent.MVar (modifyMVar, newMVar, MVar)
import Control.Monad              (mapM_)
import qualified Control.Monad.Parallel as Parallel (mapM_)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Data.HashMap.Lazy (HashMap, elems, keys)
import qualified Data.HashSet as HashSet (map)
import Data.HashSet               (HashSet, insert, toList, empty, unions)
import Data.Text.Lazy             (unpack)
import GHC.IO.Handle (Handle)
import Network (PortID(PortNumber))
import Network.Socket (HostName)
import Text.Printf (printf)
import Thrift.Protocol (Protocol)
import Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import Thrift.Transport.Handle (hOpen)




domain_name :: Participant_ID -> HostName
domain_name (Participant_ID{participant_ID_address=(Address{address_host_address=(Host_Address{host_Address_dns_name=(Just x)})})}) = unpack x
domain_name (Participant_ID{participant_ID_address=(Address{address_host_address=(Host_Address{host_Address_ipv4_address=(Just (IPv4_Address{
    iPv4_Address_byte_0 = byte_0
  , iPv4_Address_byte_1 = byte_1
  , iPv4_Address_byte_2 = byte_2
  , iPv4_Address_byte_3 = byte_3}))})})}) = (show byte_0) ++ "." ++ (show byte_1) ++ "." ++ (show byte_2) ++ "." ++ (show byte_3)
domain_name (Participant_ID{participant_ID_address=(Address{address_host_address=(Host_Address{host_Address_ipv6_address=(Just (IPv6_Address{
    iPv6_Address_byte_0 = byte_0
  , iPv6_Address_byte_1 = byte_1
  , iPv6_Address_byte_2 = byte_2
  , iPv6_Address_byte_3 = byte_3
  , iPv6_Address_byte_4 = byte_4
  , iPv6_Address_byte_5 = byte_5
  , iPv6_Address_byte_6 = byte_6
  , iPv6_Address_byte_7 = byte_7
  , iPv6_Address_byte_8 = byte_8
  , iPv6_Address_byte_9 = byte_9
  , iPv6_Address_byte_10 = byte_10
  , iPv6_Address_byte_11 = byte_11
  , iPv6_Address_byte_12 = byte_12
  , iPv6_Address_byte_13 = byte_13
  , iPv6_Address_byte_14 = byte_14
  , iPv6_Address_byte_15 = byte_15}))})})}) =(
    printf "%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x"
           byte_0 byte_1 byte_2 byte_3 byte_4 byte_5 byte_6 byte_7 byte_8 byte_9 byte_10 byte_11 byte_12 byte_13 byte_14 byte_15)


-- | An address book maintains a list (used as a stack) of open communication handles for each other participant.
-- | This is a list because, if all existing handles are busy, we will spin up a new one and later add it to the list.
type Address_Book = Concurrent_Map.Map Participant_ID (MVar [(BinaryProtocol Handle, BinaryProtocol Handle)])
default_Address_Book :: IO Address_Book
default_Address_Book = Concurrent_Map.empty



send_to :: Address_Book -> Participant_ID -> ((BinaryProtocol Handle, BinaryProtocol Handle) -> a -> IO b) ->  a -> IO b
send_to address_book recipient prompt message = do
  { client <- do { first_look <- Concurrent_Map.lookup recipient address_book
                 ; case first_look of
                     Just client -> return client
                     Nothing -> do { new_entry <- newMVar []
                                   ; insertIfAbsent recipient new_entry address_book
                                   ; second_look <- Concurrent_Map.lookup recipient address_book
                                   ; case second_look of
                                       Just client -> return client
                                       Nothing -> fail "I inserted a participant into the address book, but it's not there now for some reason!"}}
    -- ModifyMVar executes an atomic transaction on the MVar, so we want to do as little within this transaction as possible
  ; maybe_client <- modifyMVar client (\x -> case x of -- pop a handle off the stack, if possible. We will be the only thread using this handle at a given time.
                                               (h:t) -> return (t, Just h) -- pops a handle off the stack
                                               e -> return (e, Nothing)) -- all existing handles are busy
  ; client' <- case maybe_client of
                 Just c  -> return c -- If there already exists a handle that's not in use
                                -- otherwise, we'll make a new one and use that
                 Nothing -> do { handle <- hOpen (domain_name recipient, PortNumber $ fromIntegral $ address_port_number $ participant_ID_address recipient )
                               ; return (BinaryProtocol handle, BinaryProtocol handle)}
  ; x <- prompt client' message
    -- ModifyMVar executes an atomic transaction on the MVar, so we want to do as little within this transaction as possible
  ; modifyMVar client (\r -> return (client' : r, ())) -- add our handle to the stack of known handles
  ; return x}



-- | As it is conceivable that sending a message could take an arbitrary amount of time (especially as we wait for the recipient to potentially return an Exception), we send all messages in parallel, so no one waits for any other.
class (Parsable a) => Send_Message_IO a where
  send_Message_IO :: Address_Book -> (Verified a) -> IO ()

instance Send_Message_IO Recursive_1a where
  send_Message_IO address_book v1a =
    Parallel.mapM_ (\participant -> (send_to address_book participant proposal_1a $ signed v1a)) $
               toList $ unions $ toList $ unions $ elems $ extract_observer_quorums v1a

instance Send_Message_IO Recursive_1b where
  send_Message_IO address_book v1b =
    Parallel.mapM_ (\participant -> (send_to address_book participant phase_1b $ signed v1b)) $
               toList $ unions $ toList $ unions $ elems $ extract_observer_quorums v1b

instance Send_Message_IO Recursive_2a where
  send_Message_IO address_book _ = return () -- (Parallel.mapM_ $ send_Message_IO address_book) . toList . extract_1bs

instance Send_Message_IO Recursive_2b where
  send_Message_IO address_book v2b =
    Parallel.mapM_ (\observer -> (send_to address_book observer phase_2b $ signed v2b)) $
               keys $ extract_observer_quorums v2b

instance Send_Message_IO Recursive_Proof_of_Consensus where
  send_Message_IO address_book = (Parallel.mapM_ $ send_Message_IO address_book) . toList . (\(Recursive_Proof_of_Consensus v2bs) -> v2bs) . original

