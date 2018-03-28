module Hetcons.Compact_Server
    ( runCompactServer
    ) where

import Network
import System.IO
import Thrift
import Thrift.Protocol.Compact
import Thrift.Server
import Thrift.Transport.Handle()


-- | A basic threaded compact protocol socket server.
-- | This is modeled exactly on runBasicServer, which is the same, but with binary protocol.
runCompactServer :: h
               -> (h -> (CompactProtocol Handle, CompactProtocol Handle) -> IO Bool)
               -> PortNumber
               -> IO a
runCompactServer hand proc_ port = runThreadedServer compactAccept hand proc_ (PortNumber port)
  where compactAccept s = do
            (h, _, _) <- accept s
            return (CompactProtocol h, CompactProtocol h)
