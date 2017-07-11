{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A class of function input types which can be memoized, and a function which produces a memoized function version
module Hetcons.Memoize (Memoizable, memoize) where

import qualified Control.Concurrent.Map as CMap (lookup)
import Control.Concurrent.Map (empty, insertIfAbsent)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Hashable (Hashable)

-- | If a function's input type is Memoizable, the function can be memoized.
-- | For now, anything Serializable and Equatable is Memoizable in the IO monad
class Memoizable a m where
  memoize :: (a -> b) -> (m (a -> (m b)))

-- | If the Monad is IOish, and the keys are Serializable and Hashable, we memoize with a Concurrent HashMap.
instance {-# OVERLAPPABLE #-} (Eq a, Hashable a, MonadIO m) => Memoizable a m where
  memoize f = liftIO (do { v <- empty
                     ; return (\x -> liftIO (do { cached <- CMap.lookup x v
                                       ; case cached of
                                           Just y -> return y
                                           Nothing -> do { let y = f x
                                                         ; insertIfAbsent x y v
                                                         ; return y}}))})
