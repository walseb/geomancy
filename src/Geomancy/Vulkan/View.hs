{-# LANGUAGE BlockArguments #-}

module Geomancy.Vulkan.View
  ( lookAtRH
  , lookAtRH_
  ) where

import Geomancy.Mat4 (colMajor)
import Geomancy.Transform (Transform(..))
import Geomancy.Vec3 (Vec3, cross, dot, normalize, vec3, withVec3)

{- | Construct a right-handed world-to-view transformation.

This will rotate and translate the world around a static camera.
This would produce an identity when looking from @vec3 0 0 0@ to @vec3 0 0 1@ using (-Y up).

The fallback "up" should be used when you're looking close to your UP in either direction.
-}
lookAtRH
  :: Vec3 -- ^ Eye position
  -> Vec3 -- ^ Target
  -> Vec3 -- ^ Up direction
  -> Transform
lookAtRH eye target up =
  withVec3 rgt \rx ry rz ->
  withVec3 up' \ux uy uz ->
  withVec3 fwd \fx fy fz ->
  colMajor
      rx    ux    fx  0
      ry    uy    fy  0
      rz    uz    fz  0
    (-er) (-eu) (-ef) 1
  where
    fwd = normalize (target - eye)
    rgt = normalize (cross fwd up)
    up' = normalize (cross fwd rgt)
    er = dot eye rgt
    eu = dot eye up'
    ef = dot eye fwd

{- | A shortcut for using the -Y as the up axis.

Looking forward from the origin does nothing.
-}
{-# INLINE lookAtRH_ #-}
lookAtRH_ :: Vec3 -> Vec3 -> Transform
lookAtRH_ eye target = lookAtRH eye target yNeg
  where
    yNeg = vec3 0 (-1) 0
