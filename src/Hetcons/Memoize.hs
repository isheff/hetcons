{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A class of function input types which can be memoized, and a function which produces a memoized function version
module Hetcons.Memoize (Memoizable, memoize) where

import Control.Concurrent.Map     (insertIfAbsent, Map, empty, lookup)
import Data.Bits (Bits, testBit, rotateL, setBit, clearBit, shiftL)
import Data.ByteString.Lazy (ByteString, null, head, tail, pack)
import Data.Hashable (Hashable)
import Data.List (reverse)
import Data.Serialize (Serialize ,encodeLazy ,decodeLazy)
import Data.Vector (Vector, (!), fromList)
import Data.Word (Word8)
import Prelude (($), (.), (++), fromIntegral, error, Either(Left, Right), Eq, Maybe(Nothing,Just), IO, return)
import System.IO.Unsafe(unsafePerformIO)


-- | Memoize the given function by allocating a memo table,
-- and then updating the memo table on each function call.
memoIO :: (Eq a, Hashable a)
       => (a -> b)           -- ^Function to memoize
       -> IO (a -> IO b)
memoIO f = do
    v <- empty
    let f' x = do
            m <- lookup x v
            case m of
                Nothing -> do { let r = f x
                              ; insertIfAbsent x r v
                              ; return r}
                Just r  -> return r
    return f'

-- | The pure version of 'memoIO'.
memo :: (Eq a, Hashable a)
     => (a -> b)           -- ^Function to memoize
     -> (a -> b)
memo f = let f' = unsafePerformIO (memoIO f) in \ x -> unsafePerformIO (f' x)

-- | If a function's input type is Memoizable, the function can be memoized.
-- | For now, anything Serializable is Memoizable
class Memoizable a where
  memoize :: (a -> b) -> a -> b

instance {-# OVERLAPPABLE #-} (Eq a, Hashable a) => Memoizable a where
  memoize = memo
