{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Geomancy.Mat4
  ( Mat4
  , mat4
  , withMat4
  , identity
  , translate
  , scale
  , rotateX
  , rotateY
  , rotateZ
  , transpose
  , mkTransformation
  , pointwise
  , zipWith
  , toList
  , toListTrans
  ) where

import Prelude hiding (zipWith)
import GHC.Exts hiding (toList)

import Control.DeepSeq (NFData(rnf))
import Foreign (Storable(..))
import GHC.IO (IO(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import qualified Data.List as List

import Geomancy.Quaternion (Quaternion, withQuaternion)
import Geomancy.Vec3 (Vec3, withVec3)

data Mat4 = Mat4 ByteArray#

{-# INLINE mat4 #-}
mat4
  :: Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Mat4
mat4
  (F# m00) (F# m01) (F# m02) (F# m03)
  (F# m10) (F# m11) (F# m12) (F# m13)
  (F# m20) (F# m21) (F# m22) (F# m23)
  (F# m30) (F# m31) (F# m32) (F# m33) =
  runRW# \world ->
    let
      !(# world_, arr #) = newAlignedPinnedByteArray# 64# 16# world

      world00 = writeFloatArray# arr 0x0# m00 world_
      world01 = writeFloatArray# arr 0x1# m01 world00
      world02 = writeFloatArray# arr 0x2# m02 world01
      world03 = writeFloatArray# arr 0x3# m03 world02

      world10 = writeFloatArray# arr 0x4# m10 world03
      world11 = writeFloatArray# arr 0x5# m11 world10
      world12 = writeFloatArray# arr 0x6# m12 world11
      world13 = writeFloatArray# arr 0x7# m13 world12

      world20 = writeFloatArray# arr 0x8# m20 world13
      world21 = writeFloatArray# arr 0x9# m21 world20
      world22 = writeFloatArray# arr 0xA# m22 world21
      world23 = writeFloatArray# arr 0xB# m23 world22

      world30 = writeFloatArray# arr 0xC# m30 world23
      world31 = writeFloatArray# arr 0xD# m31 world30
      world32 = writeFloatArray# arr 0xE# m32 world31
      world33 = writeFloatArray# arr 0xF# m33 world32

      !(# _world', arr' #) = unsafeFreezeByteArray# arr world33
    in
      Mat4 arr'

{-# INLINE withMat4 #-}
withMat4
  :: Mat4
  ->
    ( Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      r
    )
  -> r
withMat4 (Mat4 arr) f =
  f
    (F# (indexFloatArray# arr 0x0#))
    (F# (indexFloatArray# arr 0x1#))
    (F# (indexFloatArray# arr 0x2#))
    (F# (indexFloatArray# arr 0x3#))

    (F# (indexFloatArray# arr 0x4#))
    (F# (indexFloatArray# arr 0x5#))
    (F# (indexFloatArray# arr 0x6#))
    (F# (indexFloatArray# arr 0x7#))

    (F# (indexFloatArray# arr 0x8#))
    (F# (indexFloatArray# arr 0x9#))
    (F# (indexFloatArray# arr 0xA#))
    (F# (indexFloatArray# arr 0xB#))

    (F# (indexFloatArray# arr 0xC#))
    (F# (indexFloatArray# arr 0xD#))
    (F# (indexFloatArray# arr 0xE#))
    (F# (indexFloatArray# arr 0xF#))

{-# INLINE identity #-}
identity :: Mat4
identity = mat4
  1 0 0 0
  0 1 0 0
  0 0 1 0
  0 0 0 1

{-# INLINE translate #-}
translate :: Float -> Float -> Float -> Mat4
translate x y z = mat4
  1 0 0 0
  0 1 0 0
  0 0 1 0
  x y z 1

{-# INLINE scale #-}
scale :: Float -> Float -> Float -> Mat4
scale x y z = mat4
  x 0 0 0
  0 y 0 0
  0 0 z 0
  0 0 0 1

{-# INLINE rotateX #-}
rotateX :: Float -> Mat4
rotateX rads = mat4
  1 0   0   0
  0 t11 t12 0
  0 t21 t22 0
  0 0   0   1
  where
    t11 = cost
    t12 = -sint
    t21 = sint
    t22 = cost

    cost = cos rads
    sint = sin rads

{-# INLINE rotateY #-}
rotateY :: Float -> Mat4
rotateY rads = mat4
  t00 0 t02 0
  0   1 0   0
  t20 0 t22 0
  0   0 0   1
  where
    cost = cos rads
    sint = sin rads

    t00 = cost
    t02 = sint
    t20 = -sint
    t22 = cost

{-# INLINE rotateZ #-}
rotateZ :: Float -> Mat4
rotateZ rads = mat4
  t00 t01 0 0
  t10 t11 0 0
  0   0   1 0
  0   0   0 1
  where
   t00 = cost
   t01 = -sint
   t10 = sint
   t11 = cost

   cost = cos rads
   sint = sin rads

{-# INLINE transpose #-}
transpose :: Mat4 -> Mat4
transpose =
  flip withMat4
    \ m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33 ->
    mat4
      m00 m10 m20 m30
      m01 m11 m21 m31
      m02 m12 m22 m32
      m03 m13 m23 m33

{-# INLINE mkTransformation #-}
mkTransformation :: Quaternion -> Vec3 -> Mat4
mkTransformation rs t =
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
      mat4
        (1 - 2 * (y2 + z2)) (    2 * (xy + zw))     (2 * (xz - yw)) 0
        (    2 * (xy - zw)) (1 - 2 * (x2 + z2))     (2 * (yz + xw)) 0
        (    2 * (xz + yw))     (2 * (yz - xw)) (1 - 2 * (x2 + y2)) 0
                       tx                  ty                  tz   1

pointwise :: Mat4 -> Mat4 -> (Float -> Float -> Float) -> Mat4
pointwise a b f =
  withMat4 a
    \ a00 a01 a02 a03
      a10 a11 a12 a13
      a20 a21 a22 a23
      a30 a31 a32 a33 ->
  withMat4 b
    \ b00 b01 b02 b03
      b10 b11 b12 b13
      b20 b21 b22 b23
      b30 b31 b32 b33 ->
  mat4
    (f a00 b00) (f a01 b01) (f a02 b02) (f a03 b03)
    (f a10 b10) (f a11 b11) (f a12 b12) (f a13 b13)
    (f a20 b20) (f a21 b21) (f a22 b22) (f a23 b23)
    (f a30 b30) (f a31 b31) (f a32 b32) (f a33 b33)

toList :: Mat4 -> [Float]
toList = flip withMat4
    \ a00 a01 a02 a03
      a10 a11 a12 a13
      a20 a21 a22 a23
      a30 a31 a32 a33 ->
    [ a00, a01, a02, a03
    , a10, a11, a12, a13
    , a20, a21, a22, a23
    , a30, a31, a32, a33
    ]

toListTrans :: Mat4 -> [Float]
toListTrans = flip withMat4
    \ a00 a01 a02 a03
      a10 a11 a12 a13
      a20 a21 a22 a23
      a30 a31 a32 a33 ->
    [ a00, a10, a20, a30
    , a01, a11, a21, a31
    , a02, a12, a22, a32
    , a03, a13, a23, a33
    ]

zipWith :: (Float -> Float -> c) -> Mat4 -> Mat4 -> [c]
zipWith f a b = List.zipWith f (toList a) (toList b)

foreign import ccall unsafe "M4x4_SSE" mm4sse :: Addr# -> Addr# -> Addr# -> IO ()

{-# INLINE matrixProduct #-}
matrixProduct :: Mat4 -> Mat4 -> Mat4
matrixProduct (Mat4 l) (Mat4 r) = unsafePerformIO do
  result@(Mat4 m) <- unsafeNewMat4
  mm4sse
    (byteArrayContents# l)
    (byteArrayContents# r)
    (byteArrayContents# m)
  pure result

{-# INLINE unsafeNewMat4 #-}
unsafeNewMat4 :: IO Mat4
unsafeNewMat4 =
  IO \world ->
    let
      !(# world_, arr_ #) = newAlignedPinnedByteArray# 64# 16# world
      !(# _world', arr #) = unsafeFreezeByteArray# arr_ world_
    in
      (# world, Mat4 arr #)

instance NFData Mat4 where
  rnf Mat4{} = ()

instance Semigroup Mat4 where
  {-# INLINE (<>) #-}
  (<>) = matrixProduct

instance Monoid Mat4 where
  {-# INLINE mempty #-}
  mempty = identity

instance Show Mat4 where
  show = flip withMat4
    \ m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33 ->
    unlines
      [ printf "| %.4f %.4f %.4f %.4f |" m00 m01 m02 m03
      , printf "| %.4f %.4f %.4f %.4f |" m10 m11 m12 m13
      , printf "| %.4f %.4f %.4f %.4f |" m20 m21 m22 m23
      , printf "| %.4f %.4f %.4f %.4f |" m30 m31 m32 m33
      ]

instance Storable Mat4 where
  sizeOf _mat4 = 64

  alignment _mat4 = 16

  {-# INLINE poke #-}
  poke (Ptr addr) (Mat4 arr) = IO \world ->
    let
      world' = copyByteArrayToAddr# arr 0# addr 64# world
    in
      (# world', () #)

  {-# INLINE peek #-}
  peek (Ptr addr) = IO \world ->
    let
      !(# world0, arr #)  = newAlignedPinnedByteArray# 64# 16# world
      world1              = copyAddrToByteArray# addr arr 0# 64# world0
      !(# world', arr' #) = unsafeFreezeByteArray# arr world1
    in
      (# world', Mat4 arr' #)
