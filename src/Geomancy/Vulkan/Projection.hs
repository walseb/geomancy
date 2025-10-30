module Geomancy.Vulkan.Projection
  ( reverseDepthRH
  , reverseDepthOrthoRH
  , orthoRH
  ) where

import Geomancy.Mat4 (colMajor)
import Geomancy.Transform (Transform(..))

-- | Construct a view-to-NDC transformation.
--
-- This will shove a camera frustum (an expanding pyramid) into a Vulkan "clip space" box
-- with the dimensions [-1; 1] left-to-right, [-1; 1] top-to-bottom, and [1; 0] into the screen.
--
-- That is, things further away in the view will be pulled into [0; 0; 0] point on the *back* of the box.
-- And things that on the near plane set by the argument to this function will be scaled to match
-- the aspect ratio and the field of view.
--
-- When using FoV @pi/2@ and @width@=@height the points with Z=near will keep their positions in the XY plane.
--
-- NOTE: To update your code using a vanilla perspective:
--
-- Change your depth buffer's clear value.
--    Instead of clearing to 1.0 (farthest), you now clear to 0.0.
-- Change your depth comparison function.
--    Instead of VK_COMPARE_OP_LESS, you must use VK_COMPARE_OP_GREATER.
reverseDepthRH
  :: Float
  -> Float
  -> Float -> Float
  -> Transform
reverseDepthRH fovRads zn width height =
  colMajor
    sx  0  0  0
    0  sy  0  0
    0   0  0  1
    0   0 zn  0
  where
    sx = sy * recipAspect
      where
        recipAspect = height / width
    sy = 1 / tan (fovRads / 2)

{- | Vanilla orthographic projection centered on @0,0@

@orthoRH 0 1 2 2@ gives identity transform.
@orthoRH 0 1 800 600@ will map @vec3 400 300 0@ to @vec3 0 0 0@.
-}
orthoRH :: Float -> Float -> Float -> Float -> Transform
orthoRH near far width height =
  colMajor
    sx 0 0 0
    0 sy 0 0
    0  0 z w
    0  0 0 1
  where
    sx = 2 / width
    sy = 2 / height
    z = 1 / (far - near)
    w = near * (near - far)

{- | Reverse-depth orthographic projection centered on @0,0@

Can be used in the same render pass with reverseDepthRH/VK_COMPARE_OP_GREATER pipelines.
-}
reverseDepthOrthoRH :: Float -> Float -> Float -> Float -> Transform
reverseDepthOrthoRH near far = orthoRH far near
