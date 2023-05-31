{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Word32@.

module Geomancy.UVec4
  ( UVec4
  , uvec4
  , withUVec4
  , pattern WithUVec4
  , convert
  , fromTuple
  , dot
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Coerce (Coercible, coerce)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Data.Word (Word32)
import Foreign (Storable(..))
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)
import GHC.Ix (Ix(..))

import Geomancy.Elementwise (Elementwise(..))
import Graphics.Gl.Block (Block(..))

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

{-# INLINE convert #-}
convert :: Coercible v UVec4 => (Word32 -> a) -> (a -> a -> a -> a -> r) -> v -> r
convert f t v =
  withUVec4 (coerce v) \a b c d ->
    t (f a) (f b) (f c) (f d)

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

instance Block UVec4 where
  type PackedSize UVec4 = 16
  alignment140 _  = 16
  sizeOf140 _     = 16
  alignment430    = alignment140
  sizeOf430       = sizeOf140
  isStruct _      = False
  read140     = peekDiffOff
  write140    = pokeDiffOff
  read430     = read140
  write430    = write140
  readPacked  = read140
  writePacked = write140
  {-# INLINE alignment140 #-}
  {-# INLINE sizeOf140 #-}
  {-# INLINE alignment430 #-}
  {-# INLINE sizeOf430 #-}
  {-# INLINE isStruct #-}
  {-# INLINE read140 #-}
  {-# INLINE write140 #-}
  {-# INLINE read430 #-}
  {-# INLINE write430 #-}
  {-# INLINE readPacked #-}
  {-# INLINE writePacked #-}

instance Ix UVec4 where
  {-# INLINE range #-}
  range (l, u) =
    withUVec4 l \l1 l2 l3 l4 ->
      withUVec4 u \u1 u2 u3 u4 ->
        uvec4
          <$> range (l1, u1)
          <*> range (l2, u2)
          <*> range (l3, u3)
          <*> range (l4, u4)

  {-# INLINE unsafeIndex #-}
  unsafeIndex (l, u) i =
    withUVec4 l \l1 l2 l3 l4 ->
      withUVec4 u \u1 u2 u3 u4 ->
        withUVec4 i \i1 i2 i3 i4 ->
          unsafeIndex (l4, u4) i4 + unsafeRangeSize (l4, u4) * (
          unsafeIndex (l3, u3) i3 + unsafeRangeSize (l3, u3) * (
          unsafeIndex (l2, u2) i2 + unsafeRangeSize (l2, u2) * (
          unsafeIndex (l1, u1) i1)))

  {-# INLINE inRange #-}
  inRange (l, u) i =
    withUVec4 l \l1 l2 l3 l4 ->
      withUVec4 u \u1 u2 u3 u4 ->
        withUVec4 i \i1 i2 i3 i4 ->
          inRange (l1, u1) i1 &&
          inRange (l2, u2) i2 &&
          inRange (l3, u3) i3 &&
          inRange (l4, u4) i4

{-# INLINE dot #-}
dot :: UVec4 -> UVec4 -> Word32
dot (UVec4 l1 l2 l3 l4) (UVec4 r1 r2 r3 r4) =
  l1 * r1 + l2 * r2 + l3 * r3 + l4 * r4
