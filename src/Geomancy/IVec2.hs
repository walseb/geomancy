{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Specialized and inlined @V2 Int32@.

module Geomancy.IVec2
  ( IVec2
  , ivec2
  , withIVec2
  , pattern WithIVec2
  , fromTuple
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Int (Int32)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))

import Geomancy.Elementwise (Elementwise(..))

data IVec2 = IVec2
  {-# UNPACK #-} !Int32
  {-# UNPACK #-} !Int32
  deriving (Eq, Ord, Show)

{-# INLINE ivec2 #-}
ivec2 :: Int32 -> Int32 -> IVec2
ivec2 = IVec2

{-# INLINE withIVec2 #-}
withIVec2
  :: IVec2
  -> (Int32 -> Int32 -> r)
  -> r
withIVec2 (IVec2 a b) f = f a b

pattern WithIVec2 :: Int32 -> Int32 -> IVec2
pattern WithIVec2 a b <- ((`withIVec2` (,)) -> (a, b))
{-# COMPLETE WithIVec2 #-}

{-# INLINE fromTuple #-}
fromTuple :: (Int32, Int32) -> IVec2
fromTuple (x, y) = ivec2 x y

instance NFData IVec2 where
  rnf IVec2{} = ()

type instance Element IVec2 = Int32

instance MonoFunctor IVec2 where
  {-# INLINE omap #-}
  omap f v =
    withIVec2 v \x y ->
      ivec2 (f x) (f y)

instance MonoPointed IVec2 where
  {-# INLINE opoint #-}
  opoint x = ivec2 x x

instance Elementwise IVec2 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withIVec2 p0 \x0 y0 ->
    withIVec2 p1 \x1 y1 ->
      ivec2
        (f x0 x1)
        (f y0 y1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withIVec2 p0 \x0 y0 ->
    withIVec2 p1 \x1 y1 ->
    withIVec2 p2 \x2 y2 ->
      ivec2
        (f x0 x1 x2)
        (f y0 y1 y2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withIVec2 p0 \x0 y0 ->
    withIVec2 p1 \x1 y1 ->
    withIVec2 p2 \x2 y2 ->
    withIVec2 p3 \x3 y3 ->
      ivec2
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withIVec2 p0 \x0 y0 ->
    withIVec2 p1 \x1 y1 ->
    withIVec2 p2 \x2 y2 ->
    withIVec2 p3 \x3 y3 ->
    withIVec2 p4 \x4 y4 ->
      ivec2
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)

-- XXX: That's one nasty instance...
instance Num IVec2 where
  {-# INLINE (+) #-}
  IVec2 l1 l2 + IVec2 r1 r2 =
    IVec2
      (l1 + r1)
      (l2 + r2)

  {-# INLINE (-) #-}
  IVec2 l1 l2 - IVec2 r1 r2 =
    IVec2
      (l1 - r1)
      (l2 - r2)

  {-# INLINE (*) #-}
  IVec2 l1 l2 * IVec2 r1 r2 =
    IVec2
      (l1 * r1)
      (l2 * r2)

  {-# INLINE abs #-}
  abs (IVec2 a b) =
    IVec2 (abs a) (abs b)

  {-# INLINE signum #-}
  signum v2 = withIVec2 v2 \a b ->
    ivec2 (signum a) (signum b)

  {-# INLINE fromInteger #-}
  fromInteger x = IVec2 x' x'
    where
      x' = fromInteger x

instance Storable IVec2 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 8

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v4 =
    withIVec2 v4 \a b -> do
      pokeByteOff ptr 0 a
      pokeByteOff ptr 4 b

  {-# INLINE peek #-}
  peek ptr = ivec2
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
