{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Int32@.

module Geomancy.IVec4
  ( IVec4
  , ivec4
  , withIVec4
  , pattern WithIVec4
  , convert
  , fromTuple
  , dot
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Coerce (Coercible, coerce)
import Data.Int (Int32)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)
import GHC.Ix (Ix(..))

import Geomancy.Elementwise (Elementwise(..))
import Geomancy.Gl.Block (Block(..))

data IVec4 = IVec4
  {-# UNPACK #-} !Int32
  {-# UNPACK #-} !Int32
  {-# UNPACK #-} !Int32
  {-# UNPACK #-} !Int32
  deriving (Eq, Ord, Show)

{-# INLINE ivec4 #-}
ivec4 :: Int32 -> Int32 -> Int32 -> Int32 -> IVec4
ivec4 = IVec4

{-# INLINE withIVec4 #-}
withIVec4
  :: IVec4
  -> (Int32 -> Int32 -> Int32 -> Int32 -> r)
  -> r
withIVec4 (IVec4 a b c d) f = f a b c d

pattern WithIVec4 :: Int32 -> Int32 -> Int32 -> Int32 -> IVec4
pattern WithIVec4 a b c d <- ((`withIVec4` (,,,)) -> (a, b, c, d))
{-# COMPLETE WithIVec4 #-}

{-# INLINE convert #-}
convert :: Coercible v IVec4 => (Int32 -> a) -> (a -> a -> a -> a -> r) -> v -> r
convert f t v =
  withIVec4 (coerce v) \a b c d ->
    t (f a) (f b) (f c) (f d)

{-# INLINE fromTuple #-}
fromTuple :: (Int32, Int32, Int32, Int32) -> IVec4
fromTuple (x, y, z, w) = ivec4 x y z w

{-# INLINE dot #-}
dot :: IVec4 -> IVec4 -> Int32
dot (IVec4 l1 l2 l3 l4) (IVec4 r1 r2 r3 r4) =
  l1 * r1 + l2 * r2 + l3 * r3 + l4 * r4

instance NFData IVec4 where
  rnf IVec4{} = ()

type instance Element IVec4 = Int32

instance MonoFunctor IVec4 where
  {-# INLINE omap #-}
  omap f v =
    withIVec4 v \x y z w ->
      ivec4 (f x) (f y) (f z) (f w)

instance MonoPointed IVec4 where
  opoint x = ivec4 x x x x

instance Elementwise IVec4 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withIVec4 p0 \x0 y0 z0 w0 ->
    withIVec4 p1 \x1 y1 z1 w1 ->
      ivec4
        (f x0 x1)
        (f y0 y1)
        (f z0 z1)
        (f w0 w1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withIVec4 p0 \x0 y0 z0 w0 ->
    withIVec4 p1 \x1 y1 z1 w1 ->
    withIVec4 p2 \x2 y2 z2 w2 ->
      ivec4
        (f x0 x1 x2)
        (f y0 y1 y2)
        (f z0 z1 z2)
        (f w0 w1 w2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withIVec4 p0 \x0 y0 z0 w0 ->
    withIVec4 p1 \x1 y1 z1 w1 ->
    withIVec4 p2 \x2 y2 z2 w2 ->
    withIVec4 p3 \x3 y3 z3 w3 ->
      ivec4
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)
        (f z0 z1 z2 z3)
        (f w0 w1 w2 w3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withIVec4 p0 \x0 y0 z0 w0 ->
    withIVec4 p1 \x1 y1 z1 w1 ->
    withIVec4 p2 \x2 y2 z2 w2 ->
    withIVec4 p3 \x3 y3 z3 w3 ->
    withIVec4 p4 \x4 y4 z4 w4 ->
      ivec4
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)
        (f z0 z1 z2 z3 z4)
        (f w0 w1 w2 w3 w4)

-- XXX: That's another nasty instance...
instance Num IVec4 where
  {-# INLINE (+) #-}
  IVec4 l1 l2 l3 l4 + IVec4 r1 r2 r3 r4 =
    IVec4
      (l1 + r1)
      (l2 + r2)
      (l3 + r3)
      (l4 + r4)

  {-# INLINE (-) #-}
  IVec4 l1 l2 l3 l4 - IVec4 r1 r2 r3 r4 =
    IVec4
      (l1 - r1)
      (l2 - r2)
      (l3 - r3)
      (l4 - r4)

  {-# INLINE (*) #-}
  IVec4 l1 l2 l3 l4 * IVec4 r1 r2 r3 r4 =
    IVec4
      (l1 * r1)
      (l2 * r2)
      (l3 * r3)
      (l4 * r4)

  {-# INLINE abs #-}
  abs x = x

  {-# INLINE signum #-}
  signum v4 = withIVec4 v4 \a b c d ->
    ivec4 (signum a) (signum b) (signum c) (signum d)

  {-# INLINE fromInteger #-}
  fromInteger x = IVec4 x' x' x' x'
    where
      x' = fromInteger x

instance Storable IVec4 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v4 =
    withIVec4 v4 \a b c d -> do
      pokeByteOff ptr  0 a
      pokeByteOff ptr  4 b
      pokeByteOff ptr  8 c
      pokeByteOff ptr 12 d

  {-# INLINE peek #-}
  peek ptr = ivec4
    <$> peekByteOff ptr  0
    <*> peekByteOff ptr  4
    <*> peekByteOff ptr  8
    <*> peekByteOff ptr 12

instance Block IVec4 where
  sizeOfPacked _  = 16
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
  {-# INLINE sizeOfPacked #-}
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

instance Ix IVec4 where
  {-# INLINE range #-}
  range (l, u) =
    withIVec4 l \l1 l2 l3 l4 ->
      withIVec4 u \u1 u2 u3 u4 ->
        ivec4
          <$> range (l1, u1)
          <*> range (l2, u2)
          <*> range (l3, u3)
          <*> range (l4, u4)

  {-# INLINE unsafeIndex #-}
  unsafeIndex (l, u) i =
    withIVec4 l \l1 l2 l3 l4 ->
      withIVec4 u \u1 u2 u3 u4 ->
        withIVec4 i \i1 i2 i3 i4 ->
          unsafeIndex (l4, u4) i4 + unsafeRangeSize (l4, u4) * (
          unsafeIndex (l3, u3) i3 + unsafeRangeSize (l3, u3) * (
          unsafeIndex (l2, u2) i2 + unsafeRangeSize (l2, u2) * (
          unsafeIndex (l1, u1) i1)))

  {-# INLINE inRange #-}
  inRange (l, u) i =
    withIVec4 l \l1 l2 l3 l4 ->
      withIVec4 u \u1 u2 u3 u4 ->
        withIVec4 i \i1 i2 i3 i4 ->
          inRange (l1, u1) i1 &&
          inRange (l2, u2) i2 &&
          inRange (l3, u3) i3 &&
          inRange (l4, u4) i4
