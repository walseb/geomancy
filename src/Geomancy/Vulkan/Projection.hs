module Geomancy.Vulkan.Projection
  ( perspective
  , orthoOffCenter
  ) where

import Geomancy.Mat4 (Mat4, mat4)

perspective
  :: Integral side
  => Float
  -> Float -> Float
  -> side -> side
  -> Mat4
perspective fovDegs near far width height = mat4
  t00 0   0   0
  0   t11 0   0
  0   0   t22 t23
  0   0   t32 1

  where
    t00 = f * aspectX
    t11 = negate f
    t22 = far / (near - far)
    t23 = -1
    t32 = near * far / (near - far)

    f = recip . tan $ 0.5 * fovDegs / 180 * pi

    aspectX = fromIntegral height / fromIntegral width

orthoOffCenter :: Integral side => Float -> Float -> side -> side -> Mat4
orthoOffCenter near far width height = mat4
  t00 0   0   0
  0   t11 0   0
  0   0   t22 0
  0   0   t32 1

  where
    t00 = 2 / fromIntegral width
    t11 = 2 / fromIntegral height
    t22 = 1 / negate (near - far)

    t32 = near / t22
