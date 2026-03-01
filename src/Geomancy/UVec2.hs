{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

#ifdef TH_LIFT
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
#endif

-- | Specialized and inlined @V2 Word32@.

module Geomancy.UVec2
  ( UVec2
  , uvec2
  , withUVec2
  , pattern WithUVec2
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

#ifdef TH_LIFT
import Language.Haskell.TH.Syntax (Lift)
#endif

import Geomancy.Elementwise (Elementwise(..))
import Graphics.Gl.Block (Block(..))

data UVec2 = UVec2
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  deriving (Eq, Ord, Show)
#ifdef TH_LIFT
  deriving Lift
#endif

{-# INLINE uvec2 #-}
uvec2 :: Word32 -> Word32 -> UVec2
uvec2 = UVec2

{-# INLINE withUVec2 #-}
withUVec2
  :: UVec2
  -> (Word32 -> Word32 -> r)
  -> r
withUVec2 (UVec2 a b) f = f a b

pattern WithUVec2 :: Word32 -> Word32 -> UVec2
pattern WithUVec2 a b <- ((`withUVec2` (,)) -> (a, b))
{-# COMPLETE WithUVec2 #-}

{-# INLINE convert #-}
convert :: Coercible v UVec2 => (Word32 -> a) -> (a -> a -> r) -> v -> r
convert f t v =
  withUVec2 (coerce v) \a b ->
    t (f a) (f b)

{-# INLINE fromTuple #-}
fromTuple :: (Word32, Word32) -> UVec2
fromTuple (x, y) = uvec2 x y

instance NFData UVec2 where
  rnf UVec2{} = ()

type instance Element UVec2 = Word32

instance MonoFunctor UVec2 where
  {-# INLINE omap #-}
  omap f v =
    withUVec2 v \x y ->
      uvec2 (f x) (f y)

instance MonoPointed UVec2 where
  {-# INLINE opoint #-}
  opoint x = uvec2 x x

instance Elementwise UVec2 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withUVec2 p0 \x0 y0 ->
    withUVec2 p1 \x1 y1 ->
      uvec2
        (f x0 x1)
        (f y0 y1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withUVec2 p0 \x0 y0 ->
    withUVec2 p1 \x1 y1 ->
    withUVec2 p2 \x2 y2 ->
      uvec2
        (f x0 x1 x2)
        (f y0 y1 y2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withUVec2 p0 \x0 y0 ->
    withUVec2 p1 \x1 y1 ->
    withUVec2 p2 \x2 y2 ->
    withUVec2 p3 \x3 y3 ->
      uvec2
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withUVec2 p0 \x0 y0 ->
    withUVec2 p1 \x1 y1 ->
    withUVec2 p2 \x2 y2 ->
    withUVec2 p3 \x3 y3 ->
    withUVec2 p4 \x4 y4 ->
      uvec2
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)

-- XXX: That's one nasty instance...
instance Num UVec2 where
  {-# INLINE (+) #-}
  UVec2 l1 l2 + UVec2 r1 r2 =
    UVec2
      (l1 + r1)
      (l2 + r2)

  {-# INLINE (-) #-}
  UVec2 l1 l2 - UVec2 r1 r2 =
    UVec2
      (l1 - r1)
      (l2 - r2)

  {-# INLINE (*) #-}
  UVec2 l1 l2 * UVec2 r1 r2 =
    UVec2
      (l1 * r1)
      (l2 * r2)

  {-# INLINE abs #-}
  abs (UVec2 a b) =
    UVec2 (abs a) (abs b)

  {-# INLINE signum #-}
  signum v2 = withUVec2 v2 \a b ->
    uvec2 (signum a) (signum b)

  {-# INLINE fromInteger #-}
  fromInteger x = UVec2 x' x'
    where
      x' = fromInteger x

{-# INLINE dot #-}
dot :: UVec2 -> UVec2 -> Word32
dot (UVec2 l1 l2) (UVec2 r1 r2) =
  l1 * r1 + l2 * r2

instance Storable UVec2 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 8

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v4 =
    withUVec2 v4 \a b -> do
      pokeByteOff ptr 0 a
      pokeByteOff ptr 4 b

  {-# INLINE peek #-}
  peek ptr = uvec2
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4

instance Block UVec2 where
  type PackedSize UVec2 = 8
  alignment140 _  = 8
  sizeOf140 _     = 8
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

instance Ix UVec2 where
  {-# INLINE range #-}
  range (l, u) =
    withUVec2 l \l1 l2 ->
      withUVec2 u \u1 u2 ->
        uvec2
          <$> range (l1, u1)
          <*> range (l2, u2)

  {-# INLINE unsafeIndex #-}
  unsafeIndex (l, u) i =
    withUVec2 l \l1 l2 ->
      withUVec2 u \u1 u2 ->
        withUVec2 i \i1 i2 ->
          unsafeIndex (l2, u2) i2 + unsafeRangeSize (l2, u2) *
          unsafeIndex (l1, u1) i1

  {-# INLINE inRange #-}
  inRange (l, u) i =
    withUVec2 l \l1 l2 ->
      withUVec2 u \u1 u2 ->
        withUVec2 i \i1 i2 ->
          inRange (l1, u1) i1 &&
          inRange (l2, u2) i2
