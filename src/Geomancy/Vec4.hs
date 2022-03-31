{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V4 Float@.

module Geomancy.Vec4
  ( Vec4(..)
  , vec4
  , withVec4
  , pattern WithVec4
  , fromVec2
  , fromVec22
  , fromVec3
  , fromTuple

  , (^*)
  , (^/)
  , lerp

  , dot
  , normalize

  , unsafeNewVec4
  ) where

import GHC.Exts hiding (VecCount(..), toList)

import Control.DeepSeq (NFData(rnf))
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))
import GHC.IO (IO(..))
-- import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import Geomancy.Vec2 (Vec2, withVec2)
import Geomancy.Vec3 (Vec3, withVec3)

data Vec4 = Vec4 ByteArray#

{-# INLINE vec4 #-}
vec4 :: Float -> Float -> Float -> Float -> Vec4
vec4 (F# v0) (F# v1) (F# v2) (F# v3) =
  runRW# \world ->
    let
      !(# world_, arr #) = newAlignedPinnedByteArray# 16# 16# world

      world0 = writeFloatArray# arr 0x0# v0 world_
      world1 = writeFloatArray# arr 0x1# v1 world0
      world2 = writeFloatArray# arr 0x2# v2 world1
      world3 = writeFloatArray# arr 0x3# v3 world2
      !(# _world', arr' #) = unsafeFreezeByteArray# arr world3
    in
      Vec4 arr'

{-# INLINE withVec4 #-}
withVec4
  :: Vec4
  -> (Float -> Float -> Float -> Float -> r)
  -> r
withVec4 (Vec4 arr) f =
  f
    (F# (indexFloatArray# arr 0x0#))
    (F# (indexFloatArray# arr 0x1#))
    (F# (indexFloatArray# arr 0x2#))
    (F# (indexFloatArray# arr 0x3#))

{-# INLINE compareVec4 #-}
compareVec4 :: Vec4 -> Vec4 -> Ordering
compareVec4 (Vec4 src1) (Vec4 src2) =
  compare (I# (compareByteArrays# src1 0# src2 0# 16#)) 0

instance Eq Vec4 where
  (==) a b =
    case compareVec4 a b of
      EQ -> True
      _  -> False

  (/=) a b =
    case compareVec4 a b of
      EQ -> False
      _  -> True

instance Ord Vec4 where
  compare = compareVec4

instance Show Vec4 where
  show v =
    withVec4 v $
      printf "Vec4 %.4f %.4f %.4f %.4f"

pattern WithVec4 :: Float -> Float -> Float -> Float -> Vec4
pattern WithVec4 a b c d <- ((`withVec4` (,,,)) -> (a, b, c, d))
{-# COMPLETE WithVec4 #-}

{-# INLINE fromVec2 #-}
fromVec2 :: Vec2 -> Float -> Float -> Vec4
fromVec2 xy z w =
  withVec2 xy \x y ->
    vec4 x y z w

{-# INLINE fromVec22 #-}
fromVec22 :: Vec2 -> Vec2 -> Vec4
fromVec22 xy zw =
  withVec2 xy \x y ->
  withVec2 zw \z w ->
    vec4 x y z w

{-# INLINE fromVec3 #-}
fromVec3 :: Coercible a Vec3 => a -> Float -> Vec4
fromVec3 xyz w =
  withVec3 (coerce xyz) \x y z ->
    vec4 x y z w

{-# INLINE fromTuple #-}
fromTuple :: (Float, Float, Float, Float) -> Vec4
fromTuple (x, y, z, w) = vec4 x y z w

instance NFData Vec4 where
  rnf Vec4{} = ()

type instance Element Vec4 = Float

instance MonoFunctor Vec4 where
  {-# INLINE omap #-}
  omap f v =
    withVec4 v \x y z w ->
      vec4 (f x) (f y) (f z) (f w)

instance MonoPointed Vec4 where
  opoint x = vec4 x x x x

instance Num Vec4 where
  {-# INLINE (+) #-}
  (+) l r =
    withVec4 l \l1 l2 l3 l4 ->
      withVec4 r \r1 r2 r3 r4 ->
        vec4
          (l1 + r1)
          (l2 + r2)
          (l3 + r3)
          (l4 + r4)

  {-# INLINE (-) #-}
  (-) l r =
    withVec4 l \l1 l2 l3 l4 ->
      withVec4 r \r1 r2 r3 r4 ->
        vec4
          (l1 - r1)
          (l2 - r2)
          (l3 - r3)
          (l4 - r4)

  {-# INLINE (*) #-}
  (*) l r =
    withVec4 l \l1 l2 l3 l4 ->
      withVec4 r \r1 r2 r3 r4 ->
        vec4
          (l1 * r1)
          (l2 * r2)
          (l3 * r3)
          (l4 * r4)

  {-# INLINE abs #-}
  abs v =
    withVec4 v \a b c d ->
      vec4 (abs a) (abs b) (abs c) (abs d)

  {-# INLINE signum #-}
  signum v =
    withVec4 v \a b c d ->
      vec4 (signum a) (signum b) (signum c) (signum d)

  {-# INLINE fromInteger #-}
  fromInteger x = vec4 x' x' x' x'
    where
      x' = fromInteger x

instance Fractional Vec4 where
  {-# INLINE (/) #-}
  (/) l r =
    withVec4 l \l1 l2 l3 l4 ->
      withVec4 r \r1 r2 r3 r4 ->
        vec4 (l1 / r1) (l2 / r2) (l3 / r3) (l4 / r4)

  {-# INLINE recip #-}
  recip v =
    withVec4 v \a b c d ->
      vec4 (recip a) (recip b) (recip c) (recip d)

  {-# INLINE fromRational #-}
  fromRational x = vec4 x' x' x' x'
    where
      x' = fromRational x

instance Floating Vec4 where
  pi = opoint pi

  exp = omap exp
  log = omap log
  sqrt = omap sqrt
  sin = omap sin
  cos = omap cos
  asin = omap asin
  acos = omap acos
  atan = omap atan
  sinh = omap sinh
  cosh = omap cosh
  asinh = omap asinh
  acosh = omap acosh
  atanh = omap atanh

  a ** b =
    withVec4 a \ax ay az aw ->
    withVec4 b \bx by bz bw ->
      vec4
        (ax ** bx)
        (ay ** by)
        (az ** bz)
        (aw ** bw)

instance Storable Vec4 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 16

  {-# INLINE poke #-}
  poke (Ptr addr) (Vec4 arr) = IO \world ->
    let
      world' = copyByteArrayToAddr# arr 0# addr 16# world
    in
      (# world', () #)

  {-# INLINE peek #-}
  peek (Ptr addr) = IO \world ->
    let
      !(# world0, arr #)  = newAlignedPinnedByteArray# 16# 16# world
      world1              = copyAddrToByteArray# addr arr 0# 16# world0
      !(# world', arr' #) = unsafeFreezeByteArray# arr world1
    in
      (# world', Vec4 arr' #)

-- TODO: SIMD
{-# INLINE (^*) #-}
(^*) :: Vec4 -> Float -> Vec4
(^*) v x =
  withVec4 v \a b c d ->
    vec4
      (a * x)
      (b * x)
      (c * x)
      (d * x)

{-# INLINE (^/) #-}
(^/) :: Vec4 -> Float -> Vec4
v ^/ x = v ^* recip x

{-# INLINE lerp #-}
lerp :: Float -> Vec4 -> Vec4 -> Vec4
lerp alpha u v = u ^* alpha + v ^* (1 - alpha)

-- TODO: SIMD
{-# INLINE dot #-}
dot :: Vec4 -> Vec4 -> Float
dot a b =
  withVec4 a \a1 a2 a3 a4 ->
    withVec4 b \b1 b2 b3 b4 ->
      a1 * b1 +
      a2 * b2 +
      a3 * b3 +
      a4 * b4

{-# INLINE normalize #-}
normalize :: Vec4 -> Vec4
normalize v =
  if nearZero q || nearZero (1-q) then
    v
  else
    v ^/ l

  where
    q = dot v v
    l = sqrt q

    nearZero a = abs a <= 1e-6

{-# INLINE unsafeNewVec4 #-}
unsafeNewVec4 :: IO Vec4
unsafeNewVec4 =
  IO \world ->
    let
      !(# world_, arr_ #) = newAlignedPinnedByteArray# 16# 16# world
      !(# _world', arr #) = unsafeFreezeByteArray# arr_ world_
    in
      (# world, Vec4 arr #)