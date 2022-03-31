{-# LANGUAGE BlockArguments #-}
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
  , fromTuple

  , Packed(..)
  , packed
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Int (Int32)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))

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
