{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Word32@.

module Geomancy.UVec3
  ( UVec3
  , uvec3
  , withUVec3
  , pattern WithUVec3
  , fromTuple

  , Packed(..)
  , packed
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Word (Word32)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)

import Geomancy.Elementwise (Elementwise(..))
import Geomancy.Gl.Block (Block(..))

data UVec3 = UVec3
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  deriving (Eq, Ord, Show)

{-# INLINE uvec3 #-}
uvec3 :: Word32 -> Word32 -> Word32 -> UVec3
uvec3 = UVec3

{-# INLINE withUVec3 #-}
withUVec3
  :: UVec3
  -> (Word32 -> Word32 -> Word32 -> r)
  -> r
withUVec3 (UVec3 a b c) f = f a b c

pattern WithUVec3 :: Word32 -> Word32 -> Word32 -> UVec3
pattern WithUVec3 a b c <- ((`withUVec3` (,,)) -> (a, b, c))
{-# COMPLETE WithUVec3 #-}

{-# INLINE fromTuple #-}
fromTuple :: (Word32, Word32, Word32) -> UVec3
fromTuple (a, b, c) = uvec3 a b c

instance NFData UVec3 where
  rnf UVec3{} = ()

type instance Element UVec3 = Word32

instance MonoFunctor UVec3 where
  {-# INLINE omap #-}
  omap f v =
    withUVec3 v \x y z ->
      uvec3 (f x) (f y) (f z)

instance MonoPointed UVec3 where
  {-# INLINE opoint #-}
  opoint x = uvec3 x x x

instance Elementwise UVec3 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withUVec3 p0 \x0 y0 z0 ->
    withUVec3 p1 \x1 y1 z1 ->
      uvec3
        (f x0 x1)
        (f y0 y1)
        (f z0 z1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withUVec3 p0 \x0 y0 z0 ->
    withUVec3 p1 \x1 y1 z1 ->
    withUVec3 p2 \x2 y2 z2 ->
      uvec3
        (f x0 x1 x2)
        (f y0 y1 y2)
        (f z0 z1 z2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withUVec3 p0 \x0 y0 z0 ->
    withUVec3 p1 \x1 y1 z1 ->
    withUVec3 p2 \x2 y2 z2 ->
    withUVec3 p3 \x3 y3 z3 ->
      uvec3
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)
        (f z0 z1 z2 z3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withUVec3 p0 \x0 y0 z0 ->
    withUVec3 p1 \x1 y1 z1 ->
    withUVec3 p2 \x2 y2 z2 ->
    withUVec3 p3 \x3 y3 z3 ->
    withUVec3 p4 \x4 y4 z4 ->
      uvec3
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)
        (f z0 z1 z2 z3 z4)

-- XXX: That's another nasty instance...
instance Num UVec3 where
  {-# INLINE (+) #-}
  UVec3 l1 l2 l3 + UVec3 r1 r2 r3 =
    UVec3
      (l1 + r1)
      (l2 + r2)
      (l3 + r3)

  {-# INLINE (-) #-}
  UVec3 l1 l2 l3 - UVec3 r1 r2 r3 =
    UVec3
      (l1 - r1)
      (l2 - r2)
      (l3 - r3)

  {-# INLINE (*) #-}
  UVec3 l1 l2 l3 * UVec3 r1 r2 r3 =
    UVec3
      (l1 * r1)
      (l2 * r2)
      (l3 * r3)

  {-# INLINE abs #-}
  abs x = x

  {-# INLINE signum #-}
  signum v3 = withUVec3 v3 \a b c ->
    uvec3 (signum a) (signum b) (signum c)

  {-# INLINE fromInteger #-}
  fromInteger x = UVec3 x' x' x'
    where
      x' = fromInteger x

instance Storable UVec3 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v3 =
    withUVec3 v3 \a b c -> do
      pokeByteOff ptr  0 a
      pokeByteOff ptr  4 b
      pokeByteOff ptr  8 c

  {-# INLINE peek #-}
  peek ptr = uvec3
    <$> peekByteOff ptr  0
    <*> peekByteOff ptr  4
    <*> peekByteOff ptr  8

newtype Packed = Packed { unPacked :: UVec3 }
  deriving (Eq, Ord, Show, NFData, Num)

{-# INLINE packed #-}
packed :: Word32 -> Word32 -> Word32 -> Packed
packed a b c = Packed (uvec3 a b c)

instance Storable Packed where
  {-# INLINE sizeOf #-}
  sizeOf _ = 12

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr (Packed v3) =
    withUVec3 v3 \a b c -> do
      pokeByteOff ptr 0 a
      pokeByteOff ptr 4 b
      pokeByteOff ptr 8 c

  {-# INLINE peek #-}
  peek ptr = packed
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8

instance Block UVec3 where
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
