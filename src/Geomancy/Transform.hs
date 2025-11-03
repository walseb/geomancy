{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

module Geomancy.Transform
  ( Transform(..)
  , Mat4.inverse

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

import Geomancy.Mat4 (Mat4, colMajor)
import Geomancy.Quaternion (Quaternion, withQuaternion)
import Geomancy.Vec3 (Vec3, vec3, withVec3)
import Geomancy.Vec4 (fromVec3, withVec4)
import Geomancy.Mat4 qualified as Mat4

import Graphics.Gl.Block (Block(..))

newtype Transform = Transform { unTransform :: Mat4 }
  deriving newtype (Show, Semigroup, Monoid, Storable)

instance Block Transform where
  type PackedSize Transform = 64
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

-- | Apply transformation to a vector, then normalize with perspective division
apply :: Vec3 -> Transform -> Vec3
apply = flip (!.)

{- | Matrix - row vector multiplication with perspective division

@
vOut = pv <> translate !. vIn
@
-}
(!.) :: Transform -> Vec3 -> Vec3
(!.) mat vec =
  withVec4 res \x y z w ->
    vec3 (x / w) (y / w) (z / w)
  where
    res = mat Mat4.!* fromVec3 vec 1.0

infixr 5 !.

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

-- ** Rotation

{- | Clockwise rotation around positive X axis.

Matches @\a -> rotateQ (axisAngle (vec3 1 0 0) a)@.
-}
{-# INLINE rotateX #-}
rotateX :: Float -> Transform
rotateX rads =
  colMajor
    1 0   0  0
    0 c (-s) 0
    0 s   c  0
    0 0   0  1
  where
    c = cos rads
    s = sin rads

{- | Clockwise rotation around positive Y axis.

Matches @\a -> rotateQ (axisAngle (vec3 0 1 0) a)@.
-}
{-# INLINE rotateY #-}
rotateY :: Float -> Transform
rotateY rads =
  colMajor
    c  0 s 0
    0  1 0 0
  (-s) 0 c 0
    0  0 0 1
  where
    c = cos rads
    s = sin rads

{- | Clockwise rotation around positive Z axis.

Can be used for 2D rotation in the XY plane.
Matches @\a -> rotateQ (axisAngle (vec3 0 0 1) a)@.

In the right-handed "window coordinates" (e.g. top-left corner is 0,0) "right" becomes "down" after 90deg turn.
-}
{-# INLINE rotateZ #-}
rotateZ :: Float -> Transform
rotateZ rads =
  colMajor
    c (-s) 0 0
    s   c  0 0
    0   0  1 0
    0   0  0 1
  where
   c = cos rads
   s = sin rads

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
        0                   0                   0                    1
