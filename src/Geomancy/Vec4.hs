{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ViewPatterns #-}

#ifdef TH_LIFT
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | Specialized and inlined @V4 Float@.

module Geomancy.Vec4
  ( Vec4(..)
  , vec4
  , withVec4
  , pattern WithVec4
  , convert
  , fromVec2
  , fromVec22
  , fromVec3
  , fromTuple

  , (^*)
  , (^/)
  , lerp

  , dot
  , normalize

  , newVec4
  ) where

import GHC.Exts hiding (VecCount(..), toList)

import Control.DeepSeq (NFData(rnf))
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Data.VectorSpace (VectorSpace)
import Data.VectorSpace qualified as VectorSpace
import Foreign (Storable(..))
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)
import GHC.IO (IO(..))
import GHC.OverloadedLabels (IsLabel(..))
import Text.Printf (printf)
import WebColor.Labels (IsWebColorAlpha(..))

#ifdef TH_LIFT
import Language.Haskell.TH.Syntax (Lift(..))
#endif

import Geomancy.Elementwise (Elementwise(..))
import Graphics.Gl.Block (Block(..))
import Geomancy.Gl.Funs (GlModf(..), GlNearest)
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

{-# INLINE convert #-}
convert :: Coercible v Vec4 => (Float -> a) -> (a -> a -> a -> a -> r) -> v -> r
convert f t v =
  withVec4 (coerce v) \a b c d->
    t (f a) (f b) (f c) (f d)

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

instance IsWebColorAlpha s => IsLabel s Vec4 where
  {-# INLINE fromLabel #-}
  fromLabel =
    webColorAlpha @s \r g b a ->
      vec4
        (fromIntegral r / 255)
        (fromIntegral g / 255)
        (fromIntegral b / 255)
        (fromIntegral a / 255)

instance NFData Vec4 where
  rnf Vec4{} = ()

#ifdef TH_LIFT
instance Lift Vec4 where
  lift (WithVec4 a b c d) = [| vec4 $(lift a) $(lift b) $(lift c) $(lift d) |]
  liftTyped (WithVec4 a b c d) = [|| vec4 $$(liftTyped a) $$(liftTyped b) $$(liftTyped c) $$(liftTyped d) ||]
#endif

type instance Element Vec4 = Float

instance MonoFunctor Vec4 where
  {-# INLINE omap #-}
  omap f v =
    withVec4 v \x y z w ->
      vec4 (f x) (f y) (f z) (f w)

instance MonoPointed Vec4 where
  opoint x = vec4 x x x x

instance Elementwise Vec4 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withVec4 p0 \x0 y0 z0 w0 ->
    withVec4 p1 \x1 y1 z1 w1 ->
      vec4
        (f x0 x1)
        (f y0 y1)
        (f z0 z1)
        (f w0 w1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withVec4 p0 \x0 y0 z0 w0 ->
    withVec4 p1 \x1 y1 z1 w1 ->
    withVec4 p2 \x2 y2 z2 w2 ->
      vec4
        (f x0 x1 x2)
        (f y0 y1 y2)
        (f z0 z1 z2)
        (f w0 w1 w2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withVec4 p0 \x0 y0 z0 w0 ->
    withVec4 p1 \x1 y1 z1 w1 ->
    withVec4 p2 \x2 y2 z2 w2 ->
    withVec4 p3 \x3 y3 z3 w3 ->
      vec4
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)
        (f z0 z1 z2 z3)
        (f w0 w1 w2 w3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withVec4 p0 \x0 y0 z0 w0 ->
    withVec4 p1 \x1 y1 z1 w1 ->
    withVec4 p2 \x2 y2 z2 w2 ->
    withVec4 p3 \x3 y3 z3 w3 ->
    withVec4 p4 \x4 y4 z4 w4 ->
      vec4
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)
        (f z0 z1 z2 z3 z4)
        (f w0 w1 w2 w3 w4)

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

instance Block Vec4 where
  type PackedSize Vec4 = 16
  alignment140 _  = 16
  sizeOf140       = sizeOfPacked
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

instance VectorSpace Vec4 Float where
  zeroVector = epoint 0

  {-# INLINE (*^) #-}
  (*^) = flip (Geomancy.Vec4.^*)

  {-# INLINE (^/) #-}
  (^/) = (Geomancy.Vec4.^/)

  {-# INLINE (^+^) #-}
  (^+^) = emap2 (+)

  {-# INLINE (^-^) #-}
  (^-^) = emap2 (-)

  {-# INLINE negateVector #-}
  negateVector = emap negate

  {-# INLINE dot #-}
  dot = Geomancy.Vec4.dot

  {-# INLINE normalize #-}
  normalize = Geomancy.Vec4.normalize

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

{-# INLINE newVec4 #-}
newVec4 :: IO Vec4
newVec4 =
  IO \world ->
    let
      !(# world_, arr_ #) = newAlignedPinnedByteArray# 16# 16# world
      !(# _world', arr #) = unsafeFreezeByteArray# arr_ world_
    in
      (# world, Vec4 arr #)

instance GlNearest Vec4

instance GlModf Vec4 Vec4 where
  glModf v =
    withVec4 v \vx vy vz vw ->
      let
        (xi, xf) = glModf vx
        (yi, yf) = glModf vy
        (zi, zf) = glModf vz
        (wi, wf) = glModf vw
      in
        ( vec4 (fromInteger xi) (fromInteger yi) (fromInteger zi) (fromInteger wi)
        , vec4 xf yf zf wf
        )
