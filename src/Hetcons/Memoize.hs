{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A class of function input types which can be memoized, and a function which produces a memoized function version
module Hetcons.Memoize (Memoizable, memoize) where

import Data.ByteString.Lazy (ByteString, null, head, tail, pack)
import Data.List (reverse)
import Data.Serialize (Serialize ,encodeLazy ,decodeLazy)
import Data.Vector (Vector, (!), fromList)
import Prelude (($), (.), (++), fromIntegral, error, Either(Left, Right))


-- | If a function's input type is Memoizable, the function can be memoized.
-- | For now, anything Serializable is Memoizable
class Memoizable a where
  memoize :: (a -> b) -> a -> b

-- | a helper data type for memoizing ByteStrings
data ByteTree a = ByteTree {
  leaf :: a
 ,branches :: (Vector (ByteTree a))
}

-- | ByteStrings are memoized by building an infinite tree with branching factor 256.
-- | At each node is the value of the function for the string of bytes leading to that node.
instance {-# OVERLAPPING #-} Memoizable ByteString where
  memoize f = let v h =  fromList [ h (fromIntegral i) | i <- [0..255]] -- calculate a vector of the function h applied to all Word8 s.
                  g b = ByteTree (f (pack $ reverse b)) (v (\i -> g (i:b))) -- Given a reverse-order list of bytes (efficiency), create this node of the infinite ByteTree
                  f' g' b = if null b -- This is the recursive function which navigates the infinite tree
                               then leaf g' -- return the value calculated for this point in the infinite tree
                               else f' ((branches g')!(fromIntegral $ head b)) (tail b) -- surf the tree
                  root = g [] -- the root of the infinite ByteTree
               in f' root


-- | Any Serializable object can be memoized by memoizing the ByteString encoded version of its input.
-- | As a result, if for that data type, (decode.encode) is not exactly id, the memoization will not go well.
-- | This is OVERLAPPABLE, to ensure datatype-specific memoization functions can be created, which may be more efficient.
instance {-# OVERLAPPABLE #-} Serialize a => Memoizable a where
  memoize f = (memoize (\x -> case (decodeLazy x) of
                                   Left s -> error ("decode . encode was not identity: " ++ s)
                                   Right y -> f y)
              ) . encodeLazy
