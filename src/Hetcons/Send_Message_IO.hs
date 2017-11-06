-- | The machinery for actually sending a message over the wire
module Hetcons.Send_Message_IO (Address_Book, default_Address_Book, send_Message_IO, domain_name) where

import Hetcons.Instances_Proof_of_Consensus ()
import Hetcons.Signed_Message
    ( Recursive_1b
     ,Recursive_1a
     ,Verified
       ,signed
       ,original
     ,Parsable
     ,Recursive_Proof_of_Consensus(Recursive_Proof_of_Consensus)
     ,Recursive_2b
     ,Recursive_2a
    )
import Hetcons.Value (Value, extract_observer_quorums )

import Hetcons_Observer_Client ( phase_2b )
import Hetcons_Participant_Client ( proposal_1a, phase_1b )
import Charlotte_Types
    ( Participant_ID(Participant_ID, participant_ID_address)
     ,Address(Address, address_port_number, address_host_address)
     ,Host_Address(Host_Address, host_Address_ipv6_address
                  ,host_Address_ipv4_address, host_Address_dns_name)
     ,IPv6_Address(IPv6_Address, iPv6_Address_byte_15
                  ,iPv6_Address_byte_14, iPv6_Address_byte_13, iPv6_Address_byte_12
                  ,iPv6_Address_byte_11, iPv6_Address_byte_10, iPv6_Address_byte_9
                  ,iPv6_Address_byte_8, iPv6_Address_byte_7, iPv6_Address_byte_6
                  ,iPv6_Address_byte_5, iPv6_Address_byte_4, iPv6_Address_byte_3
                  ,iPv6_Address_byte_2, iPv6_Address_byte_1, iPv6_Address_byte_0)
     ,IPv4_Address(IPv4_Address, iPv4_Address_byte_3
                  ,iPv4_Address_byte_2, iPv4_Address_byte_1, iPv4_Address_byte_0)
     ,Value_Witness)

import qualified Control.Concurrent.Map as Concurrent_Map
    ( Map, empty, lookup )
import Control.Concurrent.Map ( insertIfAbsent )
import Control.Concurrent.MVar ( modifyMVar, newMVar, MVar )
import qualified Control.Monad.Parallel as Parallel ( mapM_ )
import Data.HashMap.Lazy ( keys, elems )
import Data.HashSet ( unions, toList )
import Data.Text.Lazy ( unpack )
import GHC.IO.Handle ( Handle )
import Network ( PortID(PortNumber) )
import Network.Socket ( HostName )
import Text.Printf ( printf )
import Thrift.Protocol.Binary ( BinaryProtocol(BinaryProtocol) )
import Thrift.Transport.Handle ( hOpen )

-- | A utility function to convert a `Participant_ID` to a HostName (String).
--   If it's a domain name, this is trivial.
--   However, if it's an IP address (which we encode with bytes), we convert it up to a String format.
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
--   This is a list because, if all existing handles are busy, we will spin up a new one and later add it to the list.
type Address_Book = Concurrent_Map.Map Participant_ID (MVar [(BinaryProtocol Handle, BinaryProtocol Handle)])

-- | A default starter address book contains no handles.
default_Address_Book :: IO Address_Book
default_Address_Book = Concurrent_Map.empty



-- | Given an `Address_Book`, a recipient, a Thrift prompt (e.g. ping or proposal_1a), and an input for that prompt,
--    executes the thrift prompt (over the wire) using a tcp channel from the address book (creating a new one if necessary).
--   This is thread safe.
--   It will not block on the address book elements, and if all known handles are in use, it spins up a new one.
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
  ; putStrLn ("sending to port: " ++ (show (address_port_number $ participant_ID_address recipient)))
  ; x <- prompt client' message
  ; putStrLn ("sent to port: " ++ (show (address_port_number $ participant_ID_address recipient)))
    -- ModifyMVar executes an atomic transaction on the MVar, so we want to do as little within this transaction as possible
  ; modifyMVar client (\r -> return (client' : r, ())) -- add our handle to the stack of known handles
  ; return x}



-- | The class of all messages which can be sent over the wire.
class Send_Message_IO a where
  -- | Send this message over the wire.
  --   Note that the recipients are derived from the message itself, depending on the message's type.
  --   As it is conceivable that sending a message could take an arbitrary amount of time
  --    (especially as we wait for the recipient to potentially return an Exception),
  --    we send all messages in parallel, so no one waits for any other.
  send_Message_IO :: Address_Book -> Value_Witness -> (Verified a) -> IO ()

-- | Send a 1A to all participants listed in any quorum.
instance (Value v) => Send_Message_IO (Recursive_1a v) where
  send_Message_IO address_book witness v1a =
    Parallel.mapM_ (\participant -> (send_to address_book participant (\c (x,y) -> proposal_1a c x y) (signed v1a, witness))) $
               toList $ unions $ toList $ unions $ elems $ extract_observer_quorums v1a

-- | Send a 1B to all participants listed in any quorum.
instance (Value v) => Send_Message_IO (Recursive_1b v) where
  send_Message_IO address_book witness v1b =
    Parallel.mapM_ (\participant -> (send_to address_book participant (\c (x,y) -> phase_1b c x y) (signed v1b, witness))) $
               toList $ unions $ toList $ unions $ elems $ extract_observer_quorums v1b

-- | 2As are not actually sent over the wire.
instance (Value v) => Send_Message_IO (Recursive_2a v) where
  send_Message_IO address_book _ _ = return () -- it would be redundant, but still correct, to send: (Parallel.mapM_ $ send_Message_IO address_book) . toList . extract_1bs

-- | 2Bs are sent to each observer listed in the Quorums.
instance (Value v) => Send_Message_IO (Recursive_2b v) where
  send_Message_IO address_book _ v2b =
    Parallel.mapM_ (\participant -> (send_to address_book participant phase_2b $ signed v2b)) $
               keys $ extract_observer_quorums v2b

-- | Proofs of Consensus are not send over the wire.
instance (Value v) => Send_Message_IO (Recursive_Proof_of_Consensus v) where
  send_Message_IO address_book _ _ = return () -- it would be redundant, but still correct, to send: (Parallel.mapM_ $ send_Message_IO address_book) . toList . (\(Recursive_Proof_of_Consensus v2bs) -> v2bs) . original

