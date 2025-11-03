{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Geomancy
import Hedgehog

import Control.Monad (unless)
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import GHC.Stack (withFrozenCallStack)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Linear qualified
import Linear.Quaternion qualified
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import Geomancy.Mat4 (withRowMajor, (!*))
import Geomancy.Mat4 qualified
import Geomancy.Quaternion qualified
import Geomancy.Transform ((!.))
import Geomancy.Transform qualified as Transform
import Geomancy.Vulkan.Projection qualified as Projection
import Geomancy.Vulkan.View qualified as View

import Linear ((!*!))

main :: IO ()
main = do
  passed <- checkParallel discovered
  if passed then
    exitSuccess
  else
    exitFailure

-- | Enough tests to cover the principles
pattern PROP_TESTS :: TestLimit
pattern PROP_TESTS = 10_000

-- | Try harder to catch FP precision errors
pattern PROP_TESTS_BRUTAL :: TestLimit
pattern PROP_TESTS_BRUTAL = 10_000_000

prop_mat4_assoc :: Property
prop_mat4_assoc = withTests PROP_TESTS $ property do
  (p, p_) <- forAllTransform
  (v, v_) <- forAllTransform
  (m, m_) <- forAllTransform

  let
    pv'm = (p <> v) <> m
    p'vm = p <> (v <> m)
    delta' = nearlyEqualMat4 pv'm p'vm
  annotateShow delta'

  let
    mv_p = (m_ !*! v_) !*! p_
    m_vp = m_ !*! (v_ !*! p_)
    delta_ = nearlyEqualM44 mv_p m_vp
  annotateShow delta_

  -- Intra-library transitivity
  unless (null $ catMaybes delta') do
    -- XXX: check only if there is some outstanding error
    delta' === delta_

  -- Inter-library calculated values nearlyEqual
  [] === catMaybes (nearlyEqualM44 m_vp $ toM44 p'vm)

  [] === catMaybes (nearlyEqualM44 mv_p $ toM44 pv'm)

prop_mat4_order :: Property
prop_mat4_order = withTests 1 $ property do
  let
    cm = Geomancy.Mat4.colMajor @Geomancy.Mat4.Mat4
      0 1 2 3
      4 5 6 7
      8 9 0 1
      2 3 4 5
    rm = Geomancy.Mat4.rowMajor @Geomancy.Mat4.Mat4
      0 4 8 2
      1 5 9 3
      2 6 0 4
      3 7 1 5
  Geomancy.Mat4.toListColMajor2d cm === Geomancy.Mat4.toListColMajor2d rm

  let
    cmT = Geomancy.Mat4.transpose cm
    rmT = Geomancy.Mat4.transpose rm
  Geomancy.Mat4.toListColMajor2d cmT === Geomancy.Mat4.toListColMajor2d rmT

prop_transform_det :: Property
prop_transform_det = withTests 1 $ property do
  1     === Geomancy.Mat4.det Geomancy.Mat4.identity
  1     === Geomancy.Mat4.det (Transform.rotateY (pi/6))  -- area-preserving
  1     === Geomancy.Mat4.det (Transform.translate 1 2 3) -- area-preserving too
  0     === Geomancy.Mat4.det (Transform.scaleX 0)        -- space collapse, no inverse
  -1    === Geomancy.Mat4.det (Transform.scaleZ (-1))     -- flips the space
  8     === Geomancy.Mat4.det (Transform.scale 2)         -- stretches a little
  (1/8) === Geomancy.Mat4.det (Transform.scale (1/2))     -- stretches backwards

prop_projection_reverseDepthRH :: Property
prop_projection_reverseDepthRH = withTests 1 $ property do
  let fov90 = pi / 2
  let zNear = 0.1
  let Transform p = Projection.reverseDepthRH fov90 zNear 800 600
  annotateShow p
  let
    -- stating the expected in transposed order for testing
    Transform expected = Geomancy.Mat4.rowMajor
      0.7500 0.0000 0.0000 0.0000
      0.0000 1.0000 0.0000 0.0000
      0.0000 0.0000 0.0000  zNear
      0.0000 0.0000 1.0000 0.0000
  annotateShow expected
  let delta_ = nearlyEqualMat4 p expected
  replicate 16 Nothing === delta_

  let
    centerNearIn = vec4 0 0 zNear 1
    centerNearOut = p !* centerNearIn
    centerNearNDC = toNDC centerNearOut
  annotateShow (centerNearIn, centerNearOut)
  vec3 0 0 1 === centerNearNDC

prop_projection_orthoRH_identity :: Property
prop_projection_orthoRH_identity = withTests 1 $ property do
  let Transform pNative = Projection.orthoRH 0 1 2 2
  annotateShow pNative
  let delta_ = nearlyEqualMat4 pNative mempty
  replicate 16 Nothing === delta_

prop_projection_reverseDepthOrthoRH :: Property
prop_projection_reverseDepthOrthoRH = withTests 1 $ property do
  let t = Projection.reverseDepthOrthoRH 1 10 800 600
  annotateShow t

  vec4 0 0 1 1 === t !* vec4 0 0 1 1 -- center/near
  vec4 0 0 0 1 === t !* vec4 0 0 10 1 -- center/far
  vec4 1 1 0 1 === t !* vec4 400 300 10 1 -- edge/far
  vec4 (-1/400) (-1/300) 1 1 === t !* vec4 (-1) (-1) 1 1 -- step/near
  vec4 (1/400) (1/300) 0 1 === t !* vec4 1 1 10 1 -- step/far

toNDC :: Vec4 -> Vec3
toNDC v = withVec4 v \x y z w -> vec3 (x/w) (y/w) (z/w)

prop_view_lookAtRH_identity :: Property
prop_view_lookAtRH_identity = property do
  let alreadyThere = View.lookAtRH_ (vec3 0 0 0) (vec3 0 0 1)
  annotateShow alreadyThere

  p <- forAll genVec3

  p === Transform.apply p alreadyThere

xPos :: Vec3
xPos = vec3 1 0 0

xNeg :: Vec3
xNeg = vec3 (-1) 0 0

yPos :: Vec3
yPos = vec3 0 1 0

yNeg :: Vec3
yNeg = vec3 0 (-1) 0

zPos :: Vec3
zPos = vec3 0 0 1

zNeg :: Vec3
zNeg = vec3 0 0 (-1)

prop_transform_rotateX :: Property
prop_transform_rotateX = property do
  let angle = pi / 2
  let t = Transform.rotateX angle
  let q = Transform.rotateQ (Geomancy.Quaternion.axisAngle (vec3 1 0 0) angle)
  let yNegT = t !. zPos
  annotateShow yNegT
  let yNegQ = q !. zPos
  annotateShow yNegQ
  replicate 3 Nothing === nearlyEqualVec3 yNegT yNeg
  replicate 3 Nothing === nearlyEqualVec3 yNegQ yNeg

prop_transform_rotateY :: Property
prop_transform_rotateY = property do
  let angle = pi / 2
  let t = Transform.rotateY angle
  let q = Transform.rotateQ (Geomancy.Quaternion.axisAngle (vec3 0 1 0) angle)
  let xPosT = t !. zPos
  annotateShow xPosT
  let xPosQ = q !. zPos
  annotateShow xPosQ
  replicate 3 Nothing === nearlyEqualVec3 xPosT xPos
  replicate 3 Nothing === nearlyEqualVec3 xPosQ xPos

prop_transform_rotateZ :: Property
prop_transform_rotateZ = property do
  let angle = pi / 2
  let t = Transform.rotateZ angle
  let q = Transform.rotateQ (Geomancy.Quaternion.axisAngle (vec3 0 0 1) angle)
  let yPosT = t !. xPos
  annotateShow yPosT
  let yPosQ = q !. xPos
  annotateShow yPosQ
  replicate 3 Nothing === nearlyEqualVec3 yPosT yPos
  replicate 3 Nothing === nearlyEqualVec3 yPosQ yPos

prop_quaternion_ref :: Property
prop_quaternion_ref = property do
  annotate "Construction"
  (axis, angle) <- forAll $ (,) <$> genVec3 <*> genAngle
  let qg = Geomancy.Quaternion.axisAngle axis angle
  let ql = Linear.Quaternion.axisAngle (withVec3 axis Linear.V3) angle
  show qg === show (fromLQ ql)

  annotate "Rotation"
  p <- forAll genVec3
  let rpg = Geomancy.Quaternion.rotate qg p
  let rpl = Linear.Quaternion.rotate ql (withVec3 p Linear.V3)
  show rpg === show (fromV3 rpl)

  annotate "Multiplication"
  (axisB, angleB) <- forAll $ (,) <$> genVec3 <*> genAngle
  let qgB = Geomancy.Quaternion.axisAngle axisB angleB
  let qlB = Linear.Quaternion.axisAngle (withVec3 axisB Linear.V3) angleB
  let prodg = qg * qgB
  let prodl = ql * qlB
  show prodg === show (fromLQ prodl)

prop_quaternion_associativity :: Property
prop_quaternion_associativity = property do
  (a, b, c) <- forAll $ (,,)
    <$> (Geomancy.Quaternion.axisAngle <$> genVec3 <*> genAngle)
    <*> (Geomancy.Quaternion.axisAngle <$> genVec3 <*> genAngle)
    <*> (Geomancy.Quaternion.axisAngle <$> genVec3 <*> genAngle)
  let ab'c = (a * b) * c
  annotateShow ab'c
  let a'bc = a * (b * c)
  annotateShow a'bc
  replicate 4 Nothing === nearlyEqualQ ab'c a'bc

prop_quaternion_intrinsic :: Property
prop_quaternion_intrinsic = withTests 1 $ property do
  let yaw = pi / 2
  let pitch = pi / 2
  let roll = 0
  let q = Geomancy.Quaternion.intrinsic roll pitch yaw
  let forward' = Geomancy.Quaternion.rotate q zPos
  let right' = Geomancy.Quaternion.rotate q xPos
  let up' = Geomancy.Quaternion.rotate q yNeg
  replicate 3 Nothing === nearlyEqualVec3 yNeg forward'
  replicate 3 Nothing === nearlyEqualVec3 zNeg right'
  replicate 3 Nothing === nearlyEqualVec3 xNeg up'

prop_quaternion_extrinsic :: Property
prop_quaternion_extrinsic = withTests 1 $ property do
  let heading = pi / 2
  let elevation = pi / 2
  let tilt = 0
  let q = Geomancy.Quaternion.extrinsic heading elevation tilt
  let forward' = Geomancy.Quaternion.rotate q zPos
  let right' = Geomancy.Quaternion.rotate q xPos
  let up' = Geomancy.Quaternion.rotate q yNeg
  replicate 3 Nothing === nearlyEqualVec3 xPos forward'
  replicate 3 Nothing === nearlyEqualVec3 yPos right'
  replicate 3 Nothing === nearlyEqualVec3 zNeg up'

prop_quaternion_intrinsic_ref :: Property
prop_quaternion_intrinsic_ref = property do
  (roll, pitch, yaw) <- forAll $ (,,) <$> genAngle <*> genAngle <*> genAngle
  let fused = Geomancy.Quaternion.intrinsic roll pitch yaw
  annotateShow fused
  let composed = intrinsicComposed roll pitch yaw
  annotateShow composed
  let viaFused = Geomancy.Quaternion.rotate fused zPos
  annotateShow viaFused
  let viaComposed = Geomancy.Quaternion.rotate composed zPos
  replicate 3 Nothing === nearlyEqualVec3 viaFused viaComposed

intrinsicComposed :: Float -> Float -> Float -> Quaternion
intrinsicComposed roll pitch yaw =
  Geomancy.Quaternion.axisAngle (vec3 0 1 0) yaw *
  Geomancy.Quaternion.axisAngle (vec3 1 0 0) pitch *
  Geomancy.Quaternion.axisAngle (vec3 0 0 1) roll

forAllTransform :: PropertyT IO (Geomancy.Mat4.Mat4, Linear.M44 Float)
forAllTransform = withFrozenCallStack do
  (_name, Transform g) <- forAllWith fst genTransform
  pure (g, toM44 g)

genVec3 :: Gen Vec3
genVec3 =
  vec3
    <$> Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
    <*> Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
    <*> Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)

genAngle :: Gen Float
genAngle = Gen.float (Range.linearFracFrom 0.0 (-8*pi) (8*pi))

genTransform :: Gen ([Char], Transform) -- TODO: cross-check with Linear
genTransform = Gen.choice
  [ genIdentity
  , genTranslate
  , genRotate
  , genScale
  ]
  where
    genIdentity = pure ("identity", mempty)

    genTranslate = do
      x <- Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
      y <- Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
      z <- Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
      pure
        ( printf "translate %0.4f %0.4f %0.4f" x y z
        , Transform.translate x y z
        )

    genRotate = do
      (name, axis) <- Gen.element
        [ ("rotate/x", Transform.rotateX)
        , ("rotate/y", Transform.rotateY)
        , ("rotate/z", Transform.rotateZ)
        ]
      angle <- genAngle
      pure
        ( printf "%s %0.4f" name angle
        , axis angle
        )

    genScale = do
      x <- Gen.float (Range.linearFracFrom 1.0 1e-6 1e6)
      y <- Gen.float (Range.linearFracFrom 1.0 1e-6 1e6)
      z <- Gen.float (Range.linearFracFrom 1.0 1e-6 1e6)
      pure
        ( printf "scale %0.4f %0.4f %0.4f" x y z
        , Transform.scale3 x y z
        )

fromV3 :: Linear.V3 Float -> Vec3
fromV3 (Linear.V3 x y z) = Geomancy.vec3 x y z

fromLQ :: Linear.Quaternion.Quaternion Float -> Geomancy.Quaternion
fromLQ (Linear.Quaternion.Quaternion s (Linear.V3 x y z)) = Geomancy.quaternion s x y z

toM44 :: Geomancy.Mat4 -> Linear.M44 Float
toM44 mat4 =
  withRowMajor mat4
    \ m00 m10 m20 m30
      m01 m11 m21 m31
      m02 m12 m22 m32
      m03 m13 m23 m33 ->
        Linear.V4
          (Linear.V4 m00 m10 m20 m30)
          (Linear.V4 m01 m11 m21 m31)
          (Linear.V4 m02 m12 m22 m32)
          (Linear.V4 m03 m13 m23 m33)

nearlyEqualMat4 :: Geomancy.Mat4.Mat4 -> Geomancy.Mat4.Mat4 -> [Maybe Oops]
nearlyEqualMat4 a b = Geomancy.Mat4.zipWith nearlyEqual a b

nearlyEqualM44 :: Linear.M44 Float -> Linear.M44 Float -> [Maybe Oops]
nearlyEqualM44 a b = zipWith nearlyEqual (concatMap toList a) (concatMap toList b)

nearlyEqualVec3 :: Vec3 -> Vec3 -> [Maybe Oops]
nearlyEqualVec3 a b =
  withVec3 a \ax ay az ->
  withVec3 b \bx by bz ->
    [ nearlyEqual ax bx
    , nearlyEqual ay by
    , nearlyEqual az bz
    ]

nearlyEqualQ :: Geomancy.Quaternion -> Geomancy.Quaternion -> [Maybe Oops]
nearlyEqualQ a b =
  Geomancy.withQuaternion a \as ax ay az ->
    Geomancy.withQuaternion b \bs bx by bz ->
      [ nearlyEqual as bs
      , nearlyEqual ax bx
      , nearlyEqual ay by
      , nearlyEqual az bz
      ]

nearlyEqual :: Float -> Float -> Maybe Oops
nearlyEqual lhs rhs =
  if lhs == rhs || absDiff < 1e-6 || relDiff < 1e-4 then
    Nothing
  else
    Just Oops{..}
  where
    absDiff = abs $ lhs - rhs
    relDiff = absDiff / (abs lhs + abs rhs)
    -- fltEps = 1.19209290e-07
    -- fltMin = 1.175494e-38

data Oops = Oops {lhs :: Float, rhs :: Float, absDiff :: Float, relDiff :: Float}
  deriving (Eq, Show)

discovered :: Group
discovered = $$(discover)
