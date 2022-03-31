{-# LANGUAGE PatternSynonyms #-}

module Geomancy
  (
  -- * Vectors

  -- ** Single-precision / float32s

    Vec2
  , vec2
  , withVec2
  , pattern WithVec2
  , Point2

  , Vec3
  , vec3
  , withVec3
  , pattern WithVec3
  , Point3
  , Point3P

  , Vec4
  , vec4
  , withVec4
  , pattern WithVec4
  , Point4

  -- ** Signed / int32s

  , IVec2
  , ivec2
  , withIVec2
  , pattern WithIVec2

  , IVec3
  , ivec3
  , withIVec3
  , pattern WithIVec3

  , IVec4
  , ivec4
  , withIVec4
  , pattern WithIVec4

  -- ** Unsigned / word32s

  , UVec2
  , uvec2
  , withUVec2
  , pattern WithUVec2

  , UVec3
  , uvec3
  , withUVec3
  , pattern WithUVec3

  , UVec4
  , uvec4
  , withUVec4
  , pattern WithUVec4

  -- * Matrices

  , Mat4
  , Transform(..)

  -- * Other beasts

  , Quaternion
  , quaternion
  , withQuaternion

  -- * Spaces

  , Point(..)
  , AffineSpace(..)
  , VectorSpace(..)
  , (^*)
  , lerp
  , quadrance
  ) where

import Geomancy.Vec2 (Vec2, vec2, withVec2, pattern WithVec2)
import Geomancy.Vec3 (Vec3, vec3, withVec3, pattern WithVec3)
import Geomancy.Vec4 (Vec4, vec4, withVec4, pattern WithVec4)

import Geomancy.IVec2 (IVec2, ivec2, withIVec2, pattern WithIVec2)
import Geomancy.IVec3 (IVec3, ivec3, withIVec3, pattern WithIVec3)
import Geomancy.IVec4 (IVec4, ivec4, withIVec4, pattern WithIVec4)

import Geomancy.UVec2 (UVec2, uvec2, withUVec2, pattern WithUVec2)
import Geomancy.UVec3 (UVec3, uvec3, withUVec3, pattern WithUVec3)
import Geomancy.UVec4 (UVec4, uvec4, withUVec4, pattern WithUVec4)

import Geomancy.Mat4 (Mat4)
import Geomancy.Transform (Transform(..))

import Geomancy.Quaternion (Quaternion, quaternion, withQuaternion)

import Geomancy.Point (AffineSpace(..), Point(..), Point2, Point3, Point3P, Point4)
import Geomancy.Vector (VectorSpace(..), lerp, quadrance, (^*))
