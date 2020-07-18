{-# LANGUAGE BlockArguments #-}

-- | Specialized and inlined @Quaternion Float@.

module Geomancy.Quaternion
  ( Quaternion
  , quaternion
  , withQuaternion

  , axisAngle
  , rotate
  , rotatePoint
  , rotationBetween
  , lookAtUp

  , (^*)
  , (^/)
  , slerp

  , conjugate
  , norm
  , quadrance
  , dot
  , normalize
  , qNaN
  ) where

import Control.DeepSeq (NFData(rnf))
import Foreign (Storable(..), castPtr)

import Geomancy.Vec3 (Vec3, vec3, withVec3)

import qualified Geomancy.Vec3 as Vec3

data Quaternion = Quaternion
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  deriving (Eq, Ord, Show)

{-# INLINE quaternion #-}
quaternion :: Float -> Float -> Float -> Float -> Quaternion
quaternion = Quaternion

{-# INLINE withQuaternion #-}
withQuaternion
  :: Quaternion
  -> (Float -> Float -> Float -> Float -> r)
  -> r
withQuaternion (Quaternion a b c d) f = f a b c d

{-# INLINE (^*) #-}
(^*) :: Quaternion -> Float -> Quaternion
Quaternion a b c d ^* x =
  Quaternion
    (a * x)
    (b * x)
    (c * x)
    (d * x)

{-# INLINE (^/) #-}
(^/) :: Quaternion -> Float -> Quaternion
Quaternion a b c d ^/ x =
  Quaternion
    (a / x)
    (b / x)
    (c / x)
    (d / x)

slerp :: Quaternion -> Quaternion -> Float -> Quaternion
slerp q p t
  | 1.0 - cosphi < 1e-8 =
      q
  | otherwise =
      ( (q   ^* sin ((1 - t) * phi)) +
         f p ^* sin (t * phi)
      ) ^/ sin phi
  where
    phi = acos cosphi

    (cosphi, f) =
      if dqp < 0 then
        (-dqp, negate)
      else
        (dqp, id)

    dqp = dot q p

{-# INLINE conjugate #-}
conjugate :: Quaternion -> Quaternion
conjugate (Quaternion e x y z) = Quaternion e (-x) (-y) (-z)

{-# INLINE norm #-}
norm :: Quaternion -> Float
norm = sqrt . quadrance

{-# INLINE quadrance #-}
quadrance :: Quaternion -> Float
quadrance q = dot q q

{-# INLINE dot #-}
dot :: Quaternion -> Quaternion -> Float
dot (Quaternion a b c d) (Quaternion e f g h) =
  a * e +
  b * f +
  c * g +
  d * h -- XXX: SIMD time!

{-# INLINE normalize #-}
normalize :: Quaternion -> Quaternion
normalize v =
  if nearZero q || nearZero (1-q) then
    v
  else
    let
      Quaternion e i j k = v
    in
      Quaternion (e / l) (i / l) (j / l) (k / l)

  where
    q = dot v v
    l = sqrt q

    nearZero a = abs a <= 1e-6

instance NFData Quaternion where
  rnf Quaternion{} = ()

instance Num Quaternion where
  {-# INLINE (+) #-}
  Quaternion a b c d + Quaternion e f g h =
    Quaternion
      (a + e)
      (b + f)
      (c + g)
      (d + h)

  {-# INLINE (-) #-}
  Quaternion a b c d - Quaternion e f g h =
    Quaternion
      (a - e)
      (b - f)
      (c - g)
      (d - h)

  {-# INLINE (*) #-}
  Quaternion a b c d * Quaternion e f g h =
    withVec3 v \y z w ->
      Quaternion x y z w
    where
      x = a * e - Vec3.dot v1 v2
      v = Vec3.cross v1 v2 + v2 Vec3.^* a + v1 Vec3.^* e
      v1 = vec3 b c d
      v2 = vec3 f g h

  {-# INLINE fromInteger #-}
  fromInteger x = Quaternion (fromInteger x) 0 0 0

  {-# INLINE abs #-}
  abs z = Quaternion (norm z) 0 0 0

  {-# INLINE signum #-}
  signum q@(Quaternion e i j k)
    | m == 0 =
        q
    | not (isInfinite m || isNaN m) =
        Quaternion (e * misqrt) (i * misqrt) (j * misqrt) (k * misqrt)
    | any isNaN [e, i, j, k] = qNaN
    | not (ii || ij || ik) = Quaternion 1 0 0 0
    | not (ie || ij || ik) = Quaternion 0 1 0 0
    | not (ie || ii || ik) = Quaternion 0 0 1 0
    | not (ie || ii || ij) = Quaternion 0 0 0 1
    | otherwise = qNaN
    where
      m = quadrance q
      misqrt = recip (sqrt m)

      ie = isInfinite e
      ii = isInfinite i
      ij = isInfinite j
      ik = isInfinite k

{-# INLINE qNaN #-}
qNaN :: Quaternion
qNaN = Quaternion fNaN fNaN fNaN fNaN
  where
    fNaN = 0/0

-- XXX: GPU layouts call for some padding.
instance Storable Quaternion where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 16

  {-# INLINE poke #-}
  poke ptr (Quaternion a b c d) = do
    poke ptr' a
    pokeElemOff ptr' 1 b
    pokeElemOff ptr' 2 c
    pokeElemOff ptr' 3 d
    where
      ptr' = castPtr ptr

  {-# INLINE peek #-}
  peek ptr = Quaternion
    <$> peek ptr'
    <*> peekElemOff ptr' 1
    <*> peekElemOff ptr' 2
    <*> peekElemOff ptr' 3
    where
      ptr' = castPtr ptr

-- | Quaternion construction from axis and angle.
{-# INLINE axisAngle #-}
axisAngle :: Vec3 -> Float -> Quaternion
axisAngle axis rads =
  withVec3 (Vec3.normalize axis Vec3.^* sin half) $
    quaternion (cos half)
  where
    half = rads / 2

{-# INLINE rotate #-}
rotate :: Quaternion -> Vec3 -> Vec3
rotate q v = withQuaternion q' \_a b c d -> vec3 b c d
  where
    q' = withVec3 v \x y z ->
      q * quaternion 0 x y z * conjugate q

{-# INLINE rotatePoint #-}
rotatePoint :: Quaternion -> Vec3 -> Vec3 -> Vec3
rotatePoint q origin point =
  origin + rotate q (point - origin)

{- | Rotation between vectors.

(in other words: the quaternion needed to rotate @v1@ so that it matches @v2@)
-}
rotationBetween :: Vec3 -> Vec3 -> Quaternion
rotationBetween v1 v2 =
  if cosTheta < -1 + 0.01 then
    let
      rotationAxis1 = Vec3.cross (vec3 0 0 1) start
      rotationAxis2 = Vec3.cross (vec3 0 0 1) start
      rotationAxis =
        if Vec3.dot rotationAxis1 rotationAxis1 < 0.01 then
          rotationAxis2
        else
          rotationAxis1
    in
      axisAngle rotationAxis pi
  else
    withVec3 invAxis $ quaternion (s * 0.5)
  where
    start    = Vec3.normalize v1
    dest     = Vec3.normalize v2
    cosTheta = Vec3.dot start dest

    invAxis = Vec3.cross start dest Vec3.^/ s

    s = sqrt (2 + cosTheta * 2)

{- | Orient towards a point.

Use "rotationBetween" if you don't need to keep the object upright.
-}
lookAtUp :: Vec3 -> Vec3 -> Vec3 -> Quaternion
lookAtUp src dst up = rot2 * rot1
  where
    rot1 = rotationBetween (vec3 0 0 1) direction
    rot2 = rotationBetween newUp desiredUp

    direction = dst - src
    newUp = rotate rot1 (vec3 0 (-1) 0)

    desiredUp = Vec3.cross right direction
    right = Vec3.cross direction up
