{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Geomancy.Transform
  ( Transform(..)
  , inverse

  , apply
  , (!.)

  , translate
  , translateV

  , rotateX
  , rotateY
  , rotateZ
  , rotateQ

  , scale
  , scaleX
  , scaleY
  , scaleZ
  , scaleXY
  , scale3

  , dirPos
  ) where

import Foreign (Storable(..))
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)

import Geomancy.Mat4 (Mat4, colMajor, inverse)
import Geomancy.Quaternion (Quaternion, withQuaternion)
import Geomancy.Vec3 (Vec3, vec3, withVec3)
import Geomancy.Vec4 (fromVec3, withVec4)

import qualified Geomancy.Mat4 as Mat4
import Geomancy.Gl.Block (Block(..))

newtype Transform = Transform { unTransform :: Mat4 }
  deriving newtype (Show, Semigroup, Monoid, Storable)

instance Block Transform where
  sizeOfPacked _  = 64
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

-- | Apply transformation to a vector, then normalize with perspective division
apply :: Vec3 -> Transform -> Vec3
apply = flip (!.)

-- | Matrix - column vector multiplication with perspective division
(!.) :: Transform -> Vec3 -> Vec3
(!.) mat vec =
  withVec4 res \x y z w ->
    vec3 (x / w) (y / w) (z / w)
  where
    res = mat Mat4.!* fromVec3 vec 1.0

-- ** Translation

{-# INLINE translate #-}
translate :: Float -> Float -> Float -> Transform
translate x y z = colMajor
  1 0 0 x
  0 1 0 y
  0 0 1 z
  0 0 0 1

{-# INLINE translateV #-}
translateV :: Vec3 -> Transform
translateV vec = withVec3 vec translate

-- ** Scaling

{-# INLINE scale3 #-}
scale3 :: Float -> Float -> Float -> Transform
scale3 x y z = colMajor
  x 0 0 0
  0 y 0 0
  0 0 z 0
  0 0 0 1

{-# INLINE scale #-}
scale :: Float -> Transform
scale s = scale3 s s s

{-# INLINE scaleX #-}
scaleX :: Float -> Transform
scaleX x = scale3 x 1 1

{-# INLINE scaleY #-}
scaleY :: Float -> Transform
scaleY y = scale3 1 y 1

{-# INLINE scaleZ #-}
scaleZ :: Float -> Transform
scaleZ z = scale3 1 1 z

{-# INLINE scaleXY #-}
scaleXY :: Float -> Float -> Transform
scaleXY x y = scale3 x y 1

-- ** Euler angle rotations

{-# INLINE rotateX #-}
rotateX :: Float -> Transform
rotateX rads = colMajor
  1 0   0   0
  0 t11 t21 0
  0 t12 t22 0
  0 0   0   1
  where
    t11 = cost
    t12 = -sint
    t21 = sint
    t22 = cost

    cost = cos rads
    sint = sin rads

{-# INLINE rotateY #-}
rotateY :: Float -> Transform
rotateY rads = colMajor
  t00 0 t20 0
  0   1 0   0
  t02 0 t22 0
  0   0 0   1
  where
    cost = cos rads
    sint = sin rads

    t00 = cost
    t02 = sint
    t20 = -sint
    t22 = cost

{-# INLINE rotateZ #-}
rotateZ :: Float -> Transform
rotateZ rads = colMajor
  t00 t10 0 0
  t01 t11 0 0
  0   0   1 0
  0   0   0 1
  where
   t00 = cost
   t01 = -sint
   t10 = sint
   t11 = cost

   cost = cos rads
   sint = sin rads

{-# INLINE rotateQ #-}
rotateQ :: Quaternion -> Transform
rotateQ dir = dirPos dir 0

{-# INLINE dirPos #-}
dirPos :: Quaternion -> Vec3 -> Transform
dirPos rs t =
  withQuaternion rs \w x y z ->
  withVec3 t \tx ty tz ->
    let
      x2 = x * x
      y2 = y * y
      z2 = z * z
      xy = x * y
      xz = x * z
      xw = x * w
      yz = y * z
      yw = y * w
      zw = z * w
    in
      colMajor
        (1 - 2 * (y2 + z2)) (    2 * (xy - zw)) (    2 * (xz + yw)) tx
        (    2 * (xy + zw)) (1 - 2 * (x2 + z2)) (    2 * (yz - xw)) ty
        (    2 * (xz - yw)) (    2 * (yz + xw)) (1 - 2 * (x2 + y2)) tz
         0                   0                   0                  1
