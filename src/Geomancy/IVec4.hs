{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Int32@.

module Geomancy.IVec4
  ( IVec4
  , ivec4
  , withIVec4
  , pattern WithIVec4
  , fromTuple
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Int (Int32)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))

import Geomancy.Elementwise (Elementwise(..))

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

{-# INLINE fromTuple #-}
fromTuple :: (Int32, Int32, Int32, Int32) -> IVec4
fromTuple (x, y, z, w) = ivec4 x y z w

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
