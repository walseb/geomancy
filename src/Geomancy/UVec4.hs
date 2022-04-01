{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Word32@.

module Geomancy.UVec4
  ( UVec4
  , uvec4
  , withUVec4
  , pattern WithUVec4
  , fromTuple
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Word (Word32)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))

import Geomancy.Elementwise (Elementwise(..))

data UVec4 = UVec4
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  deriving (Eq, Ord, Show)

{-# INLINE uvec4 #-}
uvec4 :: Word32 -> Word32 -> Word32 -> Word32 -> UVec4
uvec4 = UVec4

{-# INLINE withUVec4 #-}
withUVec4
  :: UVec4
  -> (Word32 -> Word32 -> Word32 -> Word32 -> r)
  -> r
withUVec4 (UVec4 a b c d) f = f a b c d

pattern WithUVec4 :: Word32 -> Word32 -> Word32 -> Word32 -> UVec4
pattern WithUVec4 a b c d <- ((`withUVec4` (,,,)) -> (a, b, c, d))
{-# COMPLETE WithUVec4 #-}

{-# INLINE fromTuple #-}
fromTuple :: (Word32, Word32, Word32, Word32) -> UVec4
fromTuple (x, y, z, w) = uvec4 x y z w

instance NFData UVec4 where
  rnf UVec4{} = ()

type instance Element UVec4 = Word32

instance MonoFunctor UVec4 where
  {-# INLINE omap #-}
  omap f v =
    withUVec4 v \x y z w ->
      uvec4 (f x) (f y) (f z) (f w)

instance MonoPointed UVec4 where
  {-# INLINE opoint #-}
  opoint x = uvec4 x x x x

instance Elementwise UVec4 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withUVec4 p0 \x0 y0 z0 w0 ->
    withUVec4 p1 \x1 y1 z1 w1 ->
      uvec4
        (f x0 x1)
        (f y0 y1)
        (f z0 z1)
        (f w0 w1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withUVec4 p0 \x0 y0 z0 w0 ->
    withUVec4 p1 \x1 y1 z1 w1 ->
    withUVec4 p2 \x2 y2 z2 w2 ->
      uvec4
        (f x0 x1 x2)
        (f y0 y1 y2)
        (f z0 z1 z2)
        (f w0 w1 w2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withUVec4 p0 \x0 y0 z0 w0 ->
    withUVec4 p1 \x1 y1 z1 w1 ->
    withUVec4 p2 \x2 y2 z2 w2 ->
    withUVec4 p3 \x3 y3 z3 w3 ->
      uvec4
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)
        (f z0 z1 z2 z3)
        (f w0 w1 w2 w3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withUVec4 p0 \x0 y0 z0 w0 ->
    withUVec4 p1 \x1 y1 z1 w1 ->
    withUVec4 p2 \x2 y2 z2 w2 ->
    withUVec4 p3 \x3 y3 z3 w3 ->
    withUVec4 p4 \x4 y4 z4 w4 ->
      uvec4
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)
        (f z0 z1 z2 z3 z4)
        (f w0 w1 w2 w3 w4)

-- XXX: That's another nasty instance...
instance Num UVec4 where
  {-# INLINE (+) #-}
  UVec4 l1 l2 l3 l4 + UVec4 r1 r2 r3 r4 =
    UVec4
      (l1 + r1)
      (l2 + r2)
      (l3 + r3)
      (l4 + r4)

  {-# INLINE (-) #-}
  UVec4 l1 l2 l3 l4 - UVec4 r1 r2 r3 r4 =
    UVec4
      (l1 - r1)
      (l2 - r2)
      (l3 - r3)
      (l4 - r4)

  {-# INLINE (*) #-}
  UVec4 l1 l2 l3 l4 * UVec4 r1 r2 r3 r4 =
    UVec4
      (l1 * r1)
      (l2 * r2)
      (l3 * r3)
      (l4 * r4)

  {-# INLINE abs #-}
  abs x = x

  {-# INLINE signum #-}
  signum v4 = withUVec4 v4 \a b c d ->
    uvec4 (signum a) (signum b) (signum c) (signum d)

  {-# INLINE fromInteger #-}
  fromInteger x = UVec4 x' x' x' x'
    where
      x' = fromInteger x

instance Storable UVec4 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v4 =
    withUVec4 v4 \a b c d -> do
      pokeByteOff ptr  0 a
      pokeByteOff ptr  4 b
      pokeByteOff ptr  8 c
      pokeByteOff ptr 12 d

  {-# INLINE peek #-}
  peek ptr = uvec4
    <$> peekByteOff ptr  0
    <*> peekByteOff ptr  4
    <*> peekByteOff ptr  8
    <*> peekByteOff ptr 12
