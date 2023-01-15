{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | General matrix storage and operations.

module Geomancy.Mat4
  ( Mat4

  , rowMajor
  , withRowMajor
  , toListRowMajor
  , toListRowMajor2d
  , fromRowMajor2d

  , colMajor
  , withColMajor
  , toListColMajor
  , toListColMajor2d

  , identity
  , transpose
  , inverse
  , pointwise
  , zipWith
  , matrixProduct
  , scalarMultiply
  , (!*)
  ) where

import Prelude hiding (zipWith)
import GHC.Exts hiding (VecCount(..), toList)

import Control.DeepSeq (NFData(rnf))
import Foreign (Storable(..))
import GHC.IO (IO(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import qualified Data.Foldable as Foldable
import qualified Data.List as List

import Geomancy.Vec4 (Vec4(..), unsafeNewVec4)

data Mat4 = Mat4 ByteArray#

{- | Construct 'Mat4' from @row@ notation.
-}
rowMajor
  :: Coercible Mat4 a
  => Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> a
rowMajor = coerce mat4

{- | Reduce 'Mat4' with a function with @row@ notation of arguments.
-}
withRowMajor
  :: Coercible a Mat4
  => a
  ->
    ( Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      r
    )
  -> r
withRowMajor m = withMat4 (coerce m)

toListRowMajor :: Coercible a Mat4 => a -> [Float]
toListRowMajor = toList . coerce

toListRowMajor2d :: Coercible a Mat4 => a -> [[Float]]
toListRowMajor2d = toList2d . coerce

{- |
  Build a Mat4 from a list-of-lists kind of container
  with row-major ordering of elements.

@
  fromRowMajor2d (Linear.mkTransformation dir pos) :: Transform
@
-}
fromRowMajor2d
  :: forall t a
  .  ( Foldable t
     , Coercible Mat4 a
     )
  => t (t Float)
  -> Maybe a
fromRowMajor2d rows =
  case Foldable.toList rows of
    [r0, r1, r2, r3] ->
      withRow r0 \m00 m01 m02 m03 ->
      withRow r1 \m10 m11 m12 m13 ->
      withRow r2 \m20 m21 m22 m23 ->
      withRow r3 \m30 m31 m32 m33 ->
        Just . coerce $ mat4
          m00 m01 m02 m03
          m10 m11 m12 m13
          m20 m21 m22 m23
          m30 m31 m32 m33
    _ ->
      Nothing
  where
    withRow row f =
      case Foldable.toList row of
        [c0, c1, c2, c3] ->
          f c0 c1 c2 c3
        _ ->
          Nothing

{- | Construct a 'Mat4' from @column@ notation.
-}
{-# INLINE colMajor #-}
colMajor
  :: Coercible Mat4 a
  => Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> a
colMajor
  m00 m01 m02 m03
  m10 m11 m12 m13
  m20 m21 m22 m23
  m30 m31 m32 m33 =
    coerce $ mat4
      m00 m10 m20 m30
      m01 m11 m21 m31
      m02 m12 m22 m32
      m03 m13 m23 m33

{- | Reduce 'Mat4' with a function with @column@ notation for arguments.
-}
{-# INLINE withColMajor #-}
withColMajor
  :: Coercible a Mat4
  => a
  ->
    ( Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      r
    )
  -> r
withColMajor m f = withMat4 (coerce m)
  \ m00 m01 m02 m03
    m10 m11 m12 m13
    m20 m21 m22 m23
    m30 m31 m32 m33 ->
  f
    m00 m10 m20 m30
    m01 m11 m21 m31
    m02 m12 m22 m32
    m03 m13 m23 m33

toListColMajor :: Coercible a Mat4 => a -> [Float]
toListColMajor = toListTrans . coerce

toListColMajor2d :: Coercible a Mat4 => a -> [[Float]]
toListColMajor2d = toList2dTrans . coerce

{- | Construct 'Mat4' from elements in memory order.
-}
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

{- | Reduce 'Mat4' with a function with @memory@ notation for arguments.
-}
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

{- | @I@, the identity matrix.

Neutral element of its monoid, so you can use 'mempty'.
-}
{-# INLINE identity #-}
identity :: Mat4
identity = mat4
  1 0 0 0
  0 1 0 0
  0 0 1 0
  0 0 0 1

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

{- | Compute an inverse matrix, slowly.
-}
inverse :: (Coercible Mat4 a, Coercible Mat4 a) => a -> a
inverse m =
  coerce $ withMat4 (coerce m)
    \ m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33 ->
        let
          invDet = recip det

          det
            = s0 * c5
            - s1 * c4
            + s2 * c3
            + s3 * c2
            - s4 * c1
            + s5 * c0

          s0 = m00 * m11 - m10 * m01
          s1 = m00 * m12 - m10 * m02
          s2 = m00 * m13 - m10 * m03
          s3 = m01 * m12 - m11 * m02
          s4 = m01 * m13 - m11 * m03
          s5 = m02 * m13 - m12 * m03

          c5 = m22 * m33 - m32 * m23
          c4 = m21 * m33 - m31 * m23
          c3 = m21 * m32 - m31 * m22
          c2 = m20 * m33 - m30 * m23
          c1 = m20 * m32 - m30 * m22
          c0 = m20 * m31 - m30 * m21

          i00 = ( m11 * c5 - m12 * c4 + m13 * c3) * invDet
          i01 = (-m01 * c5 + m02 * c4 - m03 * c3) * invDet
          i02 = ( m31 * s5 - m32 * s4 + m33 * s3) * invDet
          i03 = (-m21 * s5 + m22 * s4 - m23 * s3) * invDet

          i10 = (-m10 * c5 + m12 * c2 - m13 * c1) * invDet
          i11 = ( m00 * c5 - m02 * c2 + m03 * c1) * invDet
          i12 = (-m30 * s5 + m32 * s2 - m33 * s1) * invDet
          i13 = ( m20 * s5 - m22 * s2 + m23 * s1) * invDet

          i20 = ( m10 * c4 - m11 * c2 + m13 * c0) * invDet
          i21 = (-m00 * c4 + m01 * c2 - m03 * c0) * invDet
          i22 = ( m30 * s4 - m31 * s2 + m33 * s0) * invDet
          i23 = (-m20 * s4 + m21 * s2 - m23 * s0) * invDet

          i30 = (-m10 * c3 + m11 * c1 - m12 * c0) * invDet
          i31 = ( m00 * c3 - m01 * c1 + m02 * c0) * invDet
          i32 = (-m30 * s3 + m31 * s1 - m32 * s0) * invDet
          i33 = ( m20 * s3 - m21 * s1 + m22 * s0) * invDet
        in
          mat4
            i00 i01 i02 i03
            i10 i11 i12 i13
            i20 i21 i22 i23
            i30 i31 i32 i33

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

toList2d :: Mat4 -> [[Float]]
toList2d = flip withMat4
    \ a00 a01 a02 a03
      a10 a11 a12 a13
      a20 a21 a22 a23
      a30 a31 a32 a33 ->
    [ [a00, a01, a02, a03]
    , [a10, a11, a12, a13]
    , [a20, a21, a22, a23]
    , [a30, a31, a32, a33]
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

toList2dTrans :: Mat4 -> [[Float]]
toList2dTrans = flip withMat4
    \ a00 a01 a02 a03
      a10 a11 a12 a13
      a20 a21 a22 a23
      a30 a31 a32 a33 ->
    [ [a00, a10, a20, a30]
    , [a01, a11, a21, a31]
    , [a02, a12, a22, a32]
    , [a03, a13, a23, a33]
    ]

zipWith :: (Float -> Float -> c) -> Mat4 -> Mat4 -> [c]
zipWith f a b = List.zipWith f (toList a) (toList b)

foreign import ccall unsafe "Mat4xMat4_SIMD" m4m4simd :: Addr# -> Addr# -> Addr# -> IO ()

{-# INLINE matrixProduct #-}
matrixProduct :: Mat4 -> Mat4 -> Mat4
matrixProduct (Mat4 l) (Mat4 r) = unsafePerformIO do
  result@(Mat4 m) <- unsafeNewMat4
  m4m4simd
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

{-# INLINE scalarMultiply #-}
scalarMultiply :: Float -> Mat4 -> Mat4
scalarMultiply x m =
  withMat4 m
    \ m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33 ->
      mat4
        (m00 * x) (m10 * x) (m20 * x) (m30 * x)
        (m01 * x) (m11 * x) (m21 * x) (m31 * x)
        (m02 * x) (m12 * x) (m22 * x) (m32 * x)
        (m03 * x) (m13 * x) (m23 * x) (m33 * x)

foreign import ccall unsafe "Mat4xVec4_SIMD" m4v4simd :: Addr# -> Addr# -> Addr# -> IO ()

-- | Matrix - column vector multiplication
(!*) :: Coercible a Mat4 => a -> Vec4 -> Vec4
(!*) (coerce -> Mat4 m) (Vec4 v) = unsafePerformIO do
  result@(Vec4 o) <- unsafeNewVec4
  m4v4simd
    (byteArrayContents# m)
    (byteArrayContents# v)
    (byteArrayContents# o)
  pure result

  -- withVec4 vec \v1 v2 v3 v4 ->
  --   withColMajor mat
  --     \ m11 m12 m13 m14
  --       m21 m22 m23 m24
  --       m31 m32 m33 m34
  --       m41 m42 m43 m44 ->
  --         vec4
  --           (m11 * v1 + m12 * v2 + m13 * v3 + m14 * v4)
  --           (m21 * v1 + m22 * v2 + m23 * v3 + m24 * v4)
  --           (m31 * v1 + m32 * v2 + m33 * v3 + m34 * v4)
  --           (m41 * v1 + m42 * v2 + m43 * v3 + m44 * v4)

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
