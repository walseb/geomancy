{-# LANGUAGE BlockArguments #-}

module Geomancy.Vulkan.View
  ( orthoFitScreen
  , lookAt
  ) where

import Geomancy.Mat4 (Mat4, rowMajor)
import Geomancy.Vec3 (Vec3, withVec3)
import Geomancy.Transform (Transform(..))

import qualified Geomancy.Vec3 as Vec3

-- | Construct a camera-view transformation with the position of the
-- camera/eye, the target (what the camera is pointing at), and an up vector
-- which determines what is "up" (typically @vec3 0 1 0@).
lookAt :: Vec3 -- ^ Eye position
       -> Vec3 -- ^ Target
       -> Vec3 -- ^ Up vector
       -> Transform
lookAt eye center up =
  withVec3 xa \xaX xaY xaZ ->
  withVec3 ya \yaX yaY yaZ ->
  withVec3 za \zaX zaY zaZ ->
  rowMajor
    xaX yaX (-zaX) 0
    xaY yaY (-zaY) 0
    xaZ yaZ (-zaZ) 0
    xd  yd    zd   1
  where
    xa = Vec3.normalize $ Vec3.cross za up
    ya = Vec3.cross xa za
    za = Vec3.normalize $ center - eye

    xd = - Vec3.dot xa eye
    yd = - Vec3.dot ya eye
    zd =   Vec3.dot za eye

orthoFitScreen :: Float -> Float -> Float -> Float -> Mat4
orthoFitScreen screenWidth screenHeight targetWidth targetHeight =
  rowMajor
    s 0 0 0
    0 s 0 0
    0 0 1 0
    0 0 0 1
  where
    s = min (screenWidth / targetWidth) (screenHeight / targetHeight)
