module Geomancy.Vulkan.Projection
  ( perspective
  , infinitePerspective
  , orthoOffCenter
  ) where

import Geomancy.Mat4 (colMajor)
import Geomancy.Transform (Transform(..))

perspective
  :: Integral side
  => Float
  -> Float -> Float
  -> side -> side
  -> Transform
perspective fovRads near far width height = colMajor
  x 0   0   0
  0 y   0   0
  0 0   z w23
  0 0 w32   1

  where
    x = cotFoV * aspectX
    y = -cotFoV
    z = far / (near - far)
    w23 = near * far / (near - far)
    w32 = -1

    cotFoV = recip . tan $ 0.5 * fovRads

    aspectX = fromIntegral height / fromIntegral width

infinitePerspective
  :: Integral side
  => Float
  -> side
  -> side
  -> Transform
infinitePerspective fovRads width height = colMajor
  x   0   0  0
  0 (-y)  0  0
  0   0 (-1) w
  0   0 (-1) 0
  where
    (x, y) =
      if width > height then
        ( cotFoV / camAspect
        , cotFoV
        )
      else
        ( cotFoV
        , cotFoV * camAspect
        )
    camAspect = fromIntegral width / fromIntegral height
    cotFoV = recip . tan $ 0.5 * fovRads

    w = -2 * near
    near = 1/128 -- 2048

orthoOffCenter :: Integral side => Float -> Float -> side -> side -> Transform
orthoOffCenter near far width height = colMajor
  x 0 0 0
  0 y 0 0
  0 0 z w
  0 0 0 1

  where
    x = 2 / fromIntegral width
    y = 2 / fromIntegral height
    z = 1 / (far - near)

    w = near * (near - far)
