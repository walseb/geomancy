{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Int32@.

module Geomancy.IVec3
  ( IVec3
  , ivec3
  , withIVec3
  , pattern WithIVec3
  , convert
  , fromTuple

  , Packed(..)
  , packed
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

data IVec3 = IVec3
  {-# UNPACK #-} !Int32
  {-# UNPACK #-} !Int32
  {-# UNPACK #-} !Int32
  deriving (Eq, Ord, Show)

{-# INLINE ivec3 #-}
ivec3 :: Int32 -> Int32 -> Int32 -> IVec3
ivec3 = IVec3

{-# INLINE withIVec3 #-}
withIVec3
  :: IVec3
  -> (Int32 -> Int32 -> Int32 -> r)
  -> r
withIVec3 (IVec3 a b c) f = f a b c

pattern WithIVec3 :: Int32 -> Int32 -> Int32 -> IVec3
pattern WithIVec3 a b c <- ((`withIVec3` (,,)) -> (a, b, c))
{-# COMPLETE WithIVec3 #-}

{-# INLINE convert #-}
convert :: Coercible v IVec3 => (Int32 -> a) -> (a -> a -> a -> r) -> v -> r
convert f t v =
  withIVec3 (coerce v) \a b c ->
    t (f a) (f b) (f c)

{-# INLINE fromTuple #-}
fromTuple :: (Int32, Int32, Int32) -> IVec3
fromTuple (a, b, c) = ivec3 a b c

instance NFData IVec3 where
  rnf IVec3{} = ()

type instance Element IVec3 = Int32

instance MonoFunctor IVec3 where
  {-# INLINE omap #-}
  omap f v =
    withIVec3 v \x y z ->
      ivec3 (f x) (f y) (f z)

instance MonoPointed IVec3 where
  opoint x = ivec3 x x x

instance Elementwise IVec3 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withIVec3 p0 \x0 y0 z0 ->
    withIVec3 p1 \x1 y1 z1 ->
      ivec3
        (f x0 x1)
        (f y0 y1)
        (f z0 z1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withIVec3 p0 \x0 y0 z0 ->
    withIVec3 p1 \x1 y1 z1 ->
    withIVec3 p2 \x2 y2 z2 ->
      ivec3
        (f x0 x1 x2)
        (f y0 y1 y2)
        (f z0 z1 z2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withIVec3 p0 \x0 y0 z0 ->
    withIVec3 p1 \x1 y1 z1 ->
    withIVec3 p2 \x2 y2 z2 ->
    withIVec3 p3 \x3 y3 z3 ->
      ivec3
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)
        (f z0 z1 z2 z3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withIVec3 p0 \x0 y0 z0 ->
    withIVec3 p1 \x1 y1 z1 ->
    withIVec3 p2 \x2 y2 z2 ->
    withIVec3 p3 \x3 y3 z3 ->
    withIVec3 p4 \x4 y4 z4 ->
      ivec3
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)
        (f z0 z1 z2 z3 z4)

-- XXX: That's another nasty instance...
instance Num IVec3 where
  {-# INLINE (+) #-}
  IVec3 l1 l2 l3 + IVec3 r1 r2 r3 =
    IVec3
      (l1 + r1)
      (l2 + r2)
      (l3 + r3)

  {-# INLINE (-) #-}
  IVec3 l1 l2 l3 - IVec3 r1 r2 r3 =
    IVec3
      (l1 - r1)
      (l2 - r2)
      (l3 - r3)

  {-# INLINE (*) #-}
  IVec3 l1 l2 l3 * IVec3 r1 r2 r3 =
    IVec3
      (l1 * r1)
      (l2 * r2)
      (l3 * r3)

  {-# INLINE abs #-}
  abs x = x

  {-# INLINE signum #-}
  signum v3 = withIVec3 v3 \a b c ->
    ivec3 (signum a) (signum b) (signum c)

  {-# INLINE fromInteger #-}
  fromInteger x = IVec3 x' x' x'
    where
      x' = fromInteger x

instance Storable IVec3 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v3 =
    withIVec3 v3 \a b c -> do
      pokeByteOff ptr  0 a
      pokeByteOff ptr  4 b
      pokeByteOff ptr  8 c

  {-# INLINE peek #-}
  peek ptr = ivec3
    <$> peekByteOff ptr  0
    <*> peekByteOff ptr  4
    <*> peekByteOff ptr  8

newtype Packed = Packed { unPacked :: IVec3 }
  deriving (Eq, Ord, Show, NFData, Num)

{-# INLINE packed #-}
packed :: Int32 -> Int32 -> Int32 -> Packed
packed a b c = Packed (ivec3 a b c)

instance Storable Packed where
  {-# INLINE sizeOf #-}
  sizeOf _ = 12

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr (Packed v3) =
    withIVec3 v3 \a b c -> do
      pokeByteOff ptr 0 a
      pokeByteOff ptr 4 b
      pokeByteOff ptr 8 c

  {-# INLINE peek #-}
  peek ptr = packed
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8

instance Block IVec3 where
  sizeOfPacked _  = 12
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

instance Block Packed where
  sizeOfPacked _  = 12
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

instance Ix IVec3 where
  {-# INLINE range #-}
  range (l, u) =
    withIVec3 l \l1 l2 l3 ->
      withIVec3 u \u1 u2 u3 ->
        ivec3
          <$> range (l1, u1)
          <*> range (l2, u2)
          <*> range (l3, u3)

  {-# INLINE unsafeIndex #-}
  unsafeIndex (l, u) i =
    withIVec3 l \l1 l2 l3 ->
      withIVec3 u \u1 u2 u3 ->
        withIVec3 i \i1 i2 i3 ->
          unsafeIndex (l3, u3) i3 + unsafeRangeSize (l3, u3) * (
          unsafeIndex (l2, u2) i2 + unsafeRangeSize (l2, u2) * (
          unsafeIndex (l1, u1) i1))

  {-# INLINE inRange #-}
  inRange (l, u) i =
    withIVec3 l \l1 l2 l3 ->
      withIVec3 u \u1 u2 u3 ->
        withIVec3 i \i1 i2 i3 ->
          inRange (l1, u1) i1 &&
          inRange (l2, u2) i2 &&
          inRange (l3, u3) i3
