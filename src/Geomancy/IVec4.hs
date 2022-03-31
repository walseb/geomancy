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
