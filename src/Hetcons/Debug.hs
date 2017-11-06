module Hetcons.Debug
  (Monad_Debuggable
     ,debug
     ,debug_print
  )
  where


-- | A compile-time setting, in effect: do we want debugging print-outs to happen?
debug_active :: Bool
debug_active = True

-- | Monads which can execute IO actions, but only for debugging purposes.
class Monad_Debuggable m where
  -- | execute an IO action
  debug :: (IO a) -> m a


-- | print out a string for debugging purposes
debug_print :: (Monad m, Monad_Debuggable m) => String -> m ()
debug_print = if debug_active
                 then debug . putStrLn
                 else \_ -> return ()
