module Geomancy.Projection
  ( lookAtRH
  , perspectiveRH
  , orthoOffCenterRH
  ) where

import Geomancy.Mat4 (Mat4, mat4)
import Geomancy.Vec3 (Vec3(..))

import qualified Geomancy.Vec3 as Vec3

lookAtRH :: Vec3 -> Vec3 -> Vec3 -> Mat4
lookAtRH eye center up =
  mat4
    xaX yaX (-zaX) 0
    xaY yaY (-zaY) 0
    xaZ yaZ (-zaZ) 0
    xd  yd    zd   1
  where
    xa@(Vec3 xaX xaY xaZ) = Vec3.normalize $ Vec3.cross za up
    ya@(Vec3 yaX yaY yaZ) = Vec3.cross xa za
    za@(Vec3 zaX zaY zaZ) = Vec3.normalize $ center Vec3.^-^ eye

    xd = - Vec3.dot xa eye
    yd = - Vec3.dot ya eye
    zd =   Vec3.dot za eye

perspectiveRH
  :: Integral side
  => Float
  -> Float -> Float
  -> side -> side
  -> Mat4
perspectiveRH fovDegs near far width height = mat4
  t00 0   0   0
  0   t11 0   0
  0   0   t22 t23
  0   0   t32 t33

  where
    t00 = f * aspectX
    t11 = negate f
    t22 = far / (near - far)
    t23 = -1
    t32 = near * far / (near - far)
    t33 = 1

    f = recip . tan $ 0.5 * fovDegs / 180 * pi

    aspectX =
      if width > height then
        fromIntegral height / fromIntegral width
      else
        1.0

orthoOffCenterRH :: Integral side => Float -> Float -> side -> side -> Mat4
orthoOffCenterRH near far width height = mat4
  t00 0   0   0
  0   t11 0   0
  0   0   t22 0
  0   0   t32 1

  where
    t00 = 2 / fromIntegral width
    t11 = 2 / (- (fromIntegral height))
    t22 = 1 / (near - far)

    t32 = near / (near - far)

    near = 0.001
    far = -1
