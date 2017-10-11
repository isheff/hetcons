import Test.Consensus (consensus_tests)
import Test.Hetcons_Exception (hetcons_exception_tests)
import Test.Signed_Message    (signed_message_tests)
import Test.Quorums           (quorums_tests)
import Test.Participant       (participant_tests)
import Test.Observer          (observer_tests)

import Control.Concurrent ( forkIO, ThreadId, threadDelay )
import Test.HUnit (runTestTT)

-- | Run the HUnit Test Lists exported by the other Test Modules
main :: IO ()
main = do { forkIO(do { putStrLn "\nHETCONS EXCEPTION TESTS"
                      ; runTestTT hetcons_exception_tests
                      ; putStrLn "\nSIGNED MESSAGE TESTS"
                      ; runTestTT signed_message_tests
                      ; putStrLn "\nQUORUMS TESTS"
                      ; runTestTT quorums_tests
                      ; putStrLn "\nPARTICIPANT TESTS"
                      ; runTestTT participant_tests
                      ; putStrLn "\nOBSERVER TESTS"
                      ; runTestTT observer_tests
                      ; putStrLn "\nCONSENSUS TESTS"
                      ; runTestTT consensus_tests
                      ; return ()
                      })
          ; threadDelay (1000*1000*90)
}
