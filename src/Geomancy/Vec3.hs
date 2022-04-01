{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Specialized and inlined @V3 Float@.

module Geomancy.Vec3
  ( Vec3
  , vec3
  , withVec3
  , pattern WithVec3
  , fromVec2
  , fromTuple

  , (^*)
  , (^/)
  , lerp

  , cross
  , dot
  , normalize

  , Packed(..)
  , packed

  , emap2
  , emap3
  , emap4
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Coerce (Coercible, coerce)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Data.VectorSpace (VectorSpace)
import Foreign (Storable(..), castPtr)
import qualified Data.VectorSpace as VectorSpace

import Geomancy.Vec2 (Vec2, withVec2)
import Geomancy.Gl.Funs
import Geomancy.Elementwise (Elementwise(..))

data Vec3 = Vec3
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  deriving (Eq, Ord, Show)

{-# INLINE vec3 #-}
vec3 :: Float -> Float -> Float -> Vec3
vec3 = Vec3

{-# INLINE withVec3 #-}
withVec3
  :: Vec3
  -> (Float -> Float -> Float -> r)
  -> r
withVec3 (Vec3 a b c) f = f a b c

pattern WithVec3 :: Float -> Float -> Float -> Vec3
pattern WithVec3 a b c <- ((`withVec3` (,,)) -> (a, b, c))
{-# COMPLETE WithVec3 #-}

{-# INLINE fromVec2 #-}
fromVec2 :: Coercible Vec3 a => Vec2 -> Float -> a
fromVec2 xy z =
  withVec2 xy \x y ->
    coerce (vec3 x y z)

{-# INLINE fromTuple #-}
fromTuple :: Coercible Vec3 a => (Float, Float, Float) -> a
fromTuple (x, y, z) = coerce (vec3 x y z)

instance NFData Vec3 where
  rnf Vec3{} = ()

type instance Element Vec3 = Float

instance MonoFunctor Vec3 where
  {-# INLINE omap #-}
  omap f v =
    withVec3 v \x y z ->
      vec3 (f x) (f y) (f z)

instance MonoPointed Vec3 where
  opoint x = vec3 x x x

instance Elementwise Vec3 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withVec3 p0 \x0 y0 z0 ->
    withVec3 p1 \x1 y1 z1 ->
      vec3
        (f x0 x1)
        (f y0 y1)
        (f z0 z1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withVec3 p0 \x0 y0 z0 ->
    withVec3 p1 \x1 y1 z1 ->
    withVec3 p2 \x2 y2 z2 ->
      vec3
        (f x0 x1 x2)
        (f y0 y1 y2)
        (f z0 z1 z2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withVec3 p0 \x0 y0 z0 ->
    withVec3 p1 \x1 y1 z1 ->
    withVec3 p2 \x2 y2 z2 ->
    withVec3 p3 \x3 y3 z3 ->
      vec3
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)
        (f z0 z1 z2 z3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withVec3 p0 \x0 y0 z0 ->
    withVec3 p1 \x1 y1 z1 ->
    withVec3 p2 \x2 y2 z2 ->
    withVec3 p3 \x3 y3 z3 ->
    withVec3 p4 \x4 y4 z4 ->
      vec3
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)
        (f z0 z1 z2 z3 z4)

instance Num Vec3 where
  {-# INLINE (+) #-}
  Vec3 a b c + Vec3 d e f =
    Vec3
      (a + d)
      (b + e)
      (c + f)

  {-# INLINE (-) #-}
  Vec3 a b c - Vec3 d e f =
    Vec3
      (a - d)
      (b - e)
      (c - f)

  {-# INLINE (*) #-}
  Vec3 a b c * Vec3 d e f =
    Vec3
      (a * d)
      (b * e)
      (c * f)

  {-# INLINE abs #-}
  abs (Vec3 a b c) =
    Vec3 (abs a) (abs b) (abs c)

  {-# INLINE signum #-}
  signum (Vec3 a b c) =
    Vec3 (signum a) (signum b) (signum c)

  {-# INLINE fromInteger #-}
  fromInteger x = Vec3 x' x' x'
    where
      x' = fromInteger x

instance Fractional Vec3 where
  {-# INLINE (/) #-}
  Vec3 l1 l2 l3 / Vec3 r1 r2 r3 =
    Vec3 (l1 / r1) (l2 / r2) (l3 / r3)

  {-# INLINE recip #-}
  recip (Vec3 a b c) =
    Vec3 (recip a) (recip b) (recip c)

  {-# INLINE fromRational #-}
  fromRational x = Vec3 x' x' x'
    where
      x' = fromRational x

instance Floating Vec3 where
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
    withVec3 a \ax ay az ->
    withVec3 b \bx by bz ->
      vec3
        (ax ** bx)
        (ay ** by)
        (az ** bz)

{-
  XXX: GPU layouts call for some padding.

  Maybe it would be worth it to flip the sizeOf-s.
-}
instance Storable Vec3 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 4

  {-# INLINE poke #-}
  poke ptr v3 =
    withVec3 v3 \a b c -> do
      poke ptr' a
      pokeElemOff ptr' 1 b
      pokeElemOff ptr' 2 c
      pokeElemOff ptr' 3 (1.0 :: Float)
    where
      ptr' = castPtr ptr

  {-# INLINE peek #-}
  peek ptr =
    vec3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where
      ptr' = castPtr ptr

{-# INLINE (^*) #-}
(^*) :: Vec3 -> Float -> Vec3
Vec3 a b c ^* x =
  Vec3
    (a * x)
    (b * x)
    (c * x)

{-# INLINE (^/) #-}
(^/) :: Vec3 -> Float -> Vec3
Vec3 a b c ^/ x =
  Vec3
    (a / x)
    (b / x)
    (c / x)

{-# INLINE lerp #-}
lerp :: Float -> Vec3 -> Vec3 -> Vec3
lerp alpha u v = u ^* alpha + v ^* (1 - alpha)

{-# INLINE cross #-}
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 d e f) =
  Vec3
    (b * f - c * e)
    (c * d - a * f)
    (a * e - b * d)

{-# INLINE dot #-}
dot :: Vec3 -> Vec3 -> Float
dot (Vec3 a b c) (Vec3 d e f) =
  a * d +
  b * e +
  c * f

{-# INLINE normalize #-}
normalize :: Vec3 -> Vec3
normalize v =
  if nearZero q || nearZero (1-q) then
    v
  else
    let
      Vec3 x y z = v
    in
      Vec3 (x / l) (y / l) (z / l)

  where
    q = dot v v
    l = sqrt q

    nearZero a = abs a <= 1e-6

instance VectorSpace Vec3 Float where
  zeroVector = 0

  {-# INLINE (*^) #-}
  (*^) = flip (Geomancy.Vec3.^*)

  {-# INLINE (^/) #-}
  (^/) = (Geomancy.Vec3.^/)

  {-# INLINE (^+^) #-}
  (^+^) = emap2 (+)

  {-# INLINE (^-^) #-}
  (^-^) = emap2 (-)

  {-# INLINE negateVector #-}
  negateVector = omap negate

  {-# INLINE dot #-}
  dot = Geomancy.Vec3.dot

  {-# INLINE normalize #-}
  normalize = Geomancy.Vec3.normalize

-- * Unpadded

type instance Element Packed = Float

newtype Packed = Packed { unPacked :: Vec3 }
  deriving stock
    ( Eq, Ord, Show
    )
  deriving newtype
    ( NFData, Num, Fractional, Floating
    , MonoFunctor, MonoPointed
    , Elementwise
    )

{-# INLINE packed #-}
packed :: Float -> Float -> Float -> Packed
packed x y z = Packed (vec3 x y z)

instance Storable Packed where
  {-# INLINE sizeOf #-}
  sizeOf _ = 12

  {-# INLINE alignment #-}
  alignment _ = 4

  {-# INLINE poke #-}
  poke ptr (Packed v3) =
    withVec3 v3 \a b c -> do
      poke ptr' a
      pokeElemOff ptr' 1 b
      pokeElemOff ptr' 2 c
    where
      ptr' = castPtr ptr

  {-# INLINE peek #-}
  peek ptr = packed
    <$> peek ptr'
    <*> peekElemOff ptr' 1
    <*> peekElemOff ptr' 2
    where
      ptr' = castPtr ptr

instance VectorSpace Packed Float where
  zeroVector = 0

  {-# INLINE (*^) #-}
  (*^) = flip $ coerce (Geomancy.Vec3.^*)

  {-# INLINE (^/) #-}
  (^/) = coerce (Geomancy.Vec3.^/)

  {-# INLINE (^+^) #-}
  v1 ^+^ v2 = v1 + v2

  {-# INLINE (^-^) #-}
  v1 ^-^ v2 = v1 - v2

  {-# INLINE negateVector #-}
  negateVector v = -v

  {-# INLINE dot #-}
  dot = coerce Geomancy.Vec3.dot

  {-# INLINE normalize #-}
  normalize = coerce Geomancy.Vec3.normalize

instance GlNearest Vec3

instance GlModf Vec3 Vec3 where
  glModf v =
    withVec3 v \vx vy vz ->
      let
        (xi, xf) = glModf vx
        (yi, yf) = glModf vy
        (zi, zf) = glModf vz
      in
        ( vec3 (fromInteger xi) (fromInteger yi) (fromInteger zi)
        , vec3 xf yf zf
        )
