{-# LANGUAGE BlockArguments #-}

-- | Specialized and inlined @Quaternion Float@.

module Geomancy.Quaternion
  ( Quaternion
  , quaternion
  , withQuaternion

  , axisAngle
  , rotate

  , conjugate
  , norm
  , quadrance
  , dot
  , normalize
  , qNaN
  ) where

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
