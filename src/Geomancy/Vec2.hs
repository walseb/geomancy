{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Float@.

module Geomancy.Vec2
  ( Vec2
  , vec2
  , withVec2
  , pattern WithVec2
  , fromTuple

  , (^*)
  , (^/)
  , lerp

  , dot
  , normalize
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Data.VectorSpace (VectorSpace)
import Foreign (Storable(..))
import qualified Data.VectorSpace as VectorSpace

import Geomancy.Gl.Funs

data Vec2 = Vec2
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  deriving (Eq, Ord, Show)

{-# INLINE vec2 #-}
vec2 :: Float -> Float -> Vec2
vec2 = Vec2

{-# INLINE withVec2 #-}
withVec2
  :: Vec2
  -> (Float -> Float -> r)
  -> r
withVec2 (Vec2 a b) f = f a b

pattern WithVec2 :: Float -> Float -> Vec2
pattern WithVec2 a b <- ((`withVec2` (,)) -> (a, b))
{-# COMPLETE WithVec2 #-}

{-# INLINE fromTuple #-}
fromTuple :: (Float, Float) -> Vec2
fromTuple (x, y) = vec2 x y

instance NFData Vec2 where
  rnf Vec2{} = ()

type instance Element Vec2 = Float

instance MonoFunctor Vec2 where
  {-# INLINE omap #-}
  omap f v =
    withVec2 v \x y ->
      vec2 (f x) (f y)

instance MonoPointed Vec2 where
  opoint x = vec2 x x

instance Num Vec2 where
  {-# INLINE (+) #-}
  Vec2 l1 l2 + Vec2 r1 r2 =
    Vec2
      (l1 + r1)
      (l2 + r2)

  {-# INLINE (-) #-}
  Vec2 l1 l2 - Vec2 r1 r2 =
    Vec2
      (l1 - r1)
      (l2 - r2)

  {-# INLINE (*) #-}
  Vec2 l1 l2 * Vec2 r1 r2 =
    Vec2
      (l1 * r1)
      (l2 * r2)

  {-# INLINE abs #-}
  abs (Vec2 a b) =
    Vec2 (abs a) (abs b)

  {-# INLINE signum #-}
  signum (Vec2 a b) =
    Vec2 (signum a) (signum b)

  {-# INLINE fromInteger #-}
  fromInteger x = Vec2 x' x'
    where
      x' = fromInteger x

instance Fractional Vec2 where
  {-# INLINE (/) #-}
  Vec2 l1 l2 / Vec2 r1 r2 =
    Vec2 (l1 / r1) (l2 / r2)

  {-# INLINE recip #-}
  recip (Vec2 a b) =
    Vec2 (recip a) (recip b)

  {-# INLINE fromRational #-}
  fromRational x = Vec2 x' x'
    where
      x' = fromRational x

instance Floating Vec2 where
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
    withVec2 a \ax ay ->
    withVec2 b \bx by ->
      vec2
        (ax ** bx)
        (ay ** by)

{-# INLINE (^*) #-}
(^*) :: Vec2 -> Float -> Vec2
Vec2 a b ^* x =
  Vec2
    (a * x)
    (b * x)

{-# INLINE (^/) #-}
(^/) :: Vec2 -> Float -> Vec2
Vec2 a b ^/ x =
  Vec2
    (a / x)
    (b / x)

{-# INLINE lerp #-}
lerp :: Float -> Vec2 -> Vec2 -> Vec2
lerp alpha u v = u ^* alpha + v ^* (1 - alpha)

{-# INLINE dot #-}
dot :: Vec2 -> Vec2 -> Float
dot (Vec2 l1 l2) (Vec2 r1 r2) =
  l1 * r1 + l2 * r2

{-# INLINE normalize #-}
normalize :: Vec2 -> Vec2
normalize v =
  if nearZero q || nearZero (1 - q) then
    v
  else
    let
      Vec2 x y = v
    in
      Vec2 (x / l) (y / l)

  where
    q = dot v v
    l = sqrt q

    nearZero a = abs a <= 1e-6

instance Storable Vec2 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 8

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v4 =
    withVec2 v4 \a b -> do
      pokeByteOff ptr 0 a
      pokeByteOff ptr 4 b

  {-# INLINE peek #-}
  peek ptr = vec2
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4

instance VectorSpace Vec2 Float where
  zeroVector = 0

  {-# INLINE (*^) #-}
  a *^ v = v Geomancy.Vec2.^* a

  {-# INLINE (^/) #-}
  v ^/ a = v Geomancy.Vec2.^/ a

  {-# INLINE (^+^) #-}
  v1 ^+^ v2 = v1 + v2

  {-# INLINE (^-^) #-}
  v1 ^-^ v2 = v1 - v2

  {-# INLINE negateVector #-}
  negateVector v = -v

  {-# INLINE dot #-}
  dot = Geomancy.Vec2.dot

  {-# INLINE normalize #-}
  normalize = Geomancy.Vec2.normalize

instance GlClamp Vec2 Vec2 where
  glMin v e =
    withVec2 v \vx vy ->
    withVec2 e \ex ey ->
      vec2
        (glMin vx ex)
        (glMin vy ey)

  glMax v e =
    withVec2 v \vx vy ->
    withVec2 e \ex ey ->
      vec2
        (glMax vx ex)
        (glMax vy ey)

instance GlClamp Float Vec2 where
  glMin v e = omap (min e) v
  glMax v e = omap (max e) v

instance GlStep Vec2 Vec2 where
  glStep edge v =
    withVec2 edge \ex ey ->
    withVec2 v \vx vy ->
      vec2
        (glStep ex vx)
        (glStep ey vy)

  glSmoothstep edge0 edge1 v =
    withVec2 edge0 \e0x e0y ->
    withVec2 edge1 \e1x e1y ->
    withVec2 v \vx vy ->
      vec2
        (glSmoothstep e0x e1x vx)
        (glSmoothstep e0y e1y vy)

  glSmootherstep edge0 edge1 v =
    withVec2 edge0 \e0x e0y ->
    withVec2 edge1 \e1x e1y ->
    withVec2 v \vx vy ->
      vec2
        (glSmootherstep e0x e1x vx)
        (glSmootherstep e0y e1y vy)

instance GlNearest Vec2 where
  glCeil  = omap glCeil
  glFloor = omap glFloor
  glRound = omap glRound
  glTrunc = omap glTrunc

instance GlModf Vec2 Vec2 where
  glModf v =
    withVec2 v \vx vy ->
      let
        (xi, xf) = glModf vx
        (yi, yf) = glModf vy
      in
        ( vec2 (fromInteger xi) (fromInteger yi)
        , vec2 xf yf
        )

instance GlMix Float Vec2 where
  glMix a b t = lerp t a b

instance GlMix Vec2 Vec2 where
  glMix a b t =
    withVec2 a \ax ay ->
    withVec2 b \bx by ->
    withVec2 t \tx ty ->
      vec2
        (glMix ax bx tx)
        (glMix ay by ty)
