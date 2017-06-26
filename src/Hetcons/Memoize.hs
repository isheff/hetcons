{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A class of function input types which can be memoized, and a function which produces a memoized function version
module Hetcons.Memoize (Memoizable, memoize, fetch, fullBigBitTree) where

import Data.Bits (Bits, testBit, rotateL, setBit, clearBit, shiftL)
import Data.ByteString.Lazy (ByteString, null, head, tail, pack)
import Data.List (reverse)
import Data.Serialize (Serialize ,encodeLazy ,decodeLazy)
import Data.Vector (Vector, (!), fromList)
import Data.Word (Word8)
import Prelude (($), (.), (++), fromIntegral, error, Either(Left, Right))


-- | If a function's input type is Memoizable, the function can be memoized.
-- | For now, anything Serializable is Memoizable
class Memoizable a where
  memoize :: (a -> b) -> a -> b

-- | a helper data type for memoizing ByteStrings
data ByteTree a = ByteTree {
  -- | stores the value for a ByteString that stops at this node
  leaf :: a
  -- | stores a sub-tree for each of the 256 possible Word8s
 ,branches :: BigBitTree (ByteTree a)
}



-- | Binary trees for looking up stuff using bit-addressable keys
class (Bits b) => BitTree t b c where
  fetch :: t -> b -> c

-- | Leaf of a BitTree
newtype BitLeaf c = BitLeaf c
instance (Bits b) => BitTree (BitLeaf c) b c where
  fetch (BitLeaf x) _ = x

-- | Node of a BitTree
data BitBranch t = BitBranch {
  zero :: t
 ,one :: t
}
instance (BitTree t b c) => BitTree (BitBranch t) b c where
  fetch bit_branch bits = if testBit bits 0
                             then fetch (one  bit_branch) (rotateL bits 1)
                             else fetch (zero bit_branch) (rotateL bits 1)

-- | BigBitTree is a binary tree that stores values to be looked up by Word8
-- | It is thus depth 8
type BigBitTree a = BitBranch (BitBranch (BitBranch (BitBranch (BitBranch (BitBranch (BitBranch (BitBranch (BitBranch (BitLeaf a)))))))))
-- We already have: instance (BitTree (BigBitTree c) Word8 c) where

-- | create a bitBranch with children that get different rightmost bits
fullBitTree :: (Bits b) => (b -> c) -> b -> (BitBranch c)
fullBitTree f x = BitBranch {
                    one  = f $ setBit   (shiftL x 1) 0
                   ,zero = f $ clearBit (shiftL x 1) 0}

-- | given a function f over Word8s, create a BigBitTree that stores (f x) for all 256 possible x.
fullBigBitTree :: (Word8 -> a) -> (BigBitTree a)
fullBigBitTree f = (fullBitTree (fullBitTree (fullBitTree (fullBitTree (fullBitTree (fullBitTree (fullBitTree (fullBitTree (fullBitTree (BitLeaf . f)))))))))) 0


-- | ByteStrings are memoized by building an infinite tree with branching factor 256.
-- | At each node is the value of the function for the string of bytes leading to that node.
instance {-# OVERLAPPING #-} Memoizable ByteString where
  memoize f = let v h =  fullBigBitTree h -- calculate a vector of the function h applied to all Word8 s.
                  g b = ByteTree (f (pack $ reverse b)) (v (\i -> g (i:b))) -- Given a reverse-order list of bytes (efficiency), create this node of the infinite ByteTree
                  f' g' b = if null b -- This is the recursive function which navigates the infinite tree
                               then leaf g' -- return the value calculated for this point in the infinite tree
                               else f' (fetch (branches g') ((fromIntegral $ head b) :: Word8)) (tail b) -- surf the tree
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
