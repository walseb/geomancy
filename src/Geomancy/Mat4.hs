{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- XXX: until there are order-specific types

-- | General matrix storage and operations.

module Geomancy.Mat4
  ( Mat4(..)
    -- * Operations
  , identity
  , transpose
  , inverse
  , det
  , pointwise
  , zipWith
  , matrixProduct
  , scalarMultiply
  , (!*)

    -- * Construction

    -- ** Col-major
  , colMajor
  , withColMajor
  , toListColMajor
  , toListColMajor2d
  , showColumns

    -- ** Row-major
  , rowMajor
  , withRowMajor
  , toListRowMajor
  , toListRowMajor2d
  , fromRowMajor2d
  , showRows
  ) where

import Prelude hiding (zipWith)
import GHC.Exts hiding (VecCount(..), toList)
import Geomancy.Mat4.Internal

import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import qualified Data.Foldable as Foldable

import Geomancy.Vec4 (Vec4(..), newVec4)

toListColMajor :: Coercible a Mat4 => a -> [Float]
toListColMajor = toListTrans . coerce

toListColMajor2d :: Coercible a Mat4 => a -> [[Float]]
toListColMajor2d = toList2dTrans . coerce

{- | Construct 'Mat4' from @row@ notation.
-}
rowMajor
  :: Coercible Mat4 a
  => Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> a
rowMajor
  e0 e1 e2 e3
  e4 e5 e6 e7
  e8 e9 eA eB
  eC eD eE eF =
    -- just store
    coerce $ fromMemory
      e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF

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
      withRow r0 \e0 e1 e2 e3 ->
      withRow r1 \e4 e5 e6 e7 ->
      withRow r2 \e8 e9 eA eB ->
      withRow r3 \eC eD eE eF ->
        Just . coerce @a $ rowMajor
          e0 e1 e2 e3
          e4 e5 e6 e7
          e8 e9 eA eB
          eC eD eE eF
    _ ->
      Nothing
  where
    withRow row f =
      case Foldable.toList row of
        [c0, c1, c2, c3] ->
          f c0 c1 c2 c3
        _ ->
          Nothing

{- | Reduce 'Mat4' with a function with @row@ notation for arguments.
-}
{-# INLINE withRowMajor #-}
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
withRowMajor m f = withMemory (coerce m)
  \ e0 e1 e2 e3
    e4 e5 e6 e7
    e8 e9 eA eB
    eC eD eE eF ->
  f -- preserved element order
    e0 e1 e2 e3
    e4 e5 e6 e7
    e8 e9 eA eB
    eC eD eE eF

{- | Construct a 'Mat4' from @column@ notation.
-}
{-# INLINE colMajor #-}
colMajor
  :: forall a . Coercible Mat4 a
  => Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> a
colMajor
  e0 e4 e8 eC
  e1 e5 e9 eD
  e2 e6 eA eE
  e3 e7 eB eF =
    coerce $ fromMemory
      e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF

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
withColMajor m f = withMemory (coerce m)
  \ e0 e4 e8 eC
    e1 e5 e9 eD
    e2 e6 eA eE
    e3 e7 eB eF ->
  f -- transposed element order
    e0 e1 e2 e3
    e4 e5 e6 e7
    e8 e9 eA eB
    eC eD eE eF

toListRowMajor :: Coercible a Mat4 => a -> [Float]
toListRowMajor = toListMemory . coerce

toListRowMajor2d :: Coercible a Mat4 => a -> [[Float]]
toListRowMajor2d = toList2dMemory . coerce

det :: forall a . (Coercible Mat4 a, Coercible Mat4 a) => a -> Float
det m =
  withRowMajor (coerce @_ @a m)
    \ m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33 ->
        let
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
        in
            s0 * c5
          - s1 * c4
          + s2 * c3
          + s3 * c2
          - s4 * c1
          + s5 * c0

{- | Compute an inverse matrix, slowly.
-}
inverse :: forall a . (Coercible Mat4 a, Coercible Mat4 a) => a -> a
inverse m =
  coerce @a $ withRowMajor (coerce @_ @a m)
    \ m00 m01 m02 m03
      m10 m11 m12 m13
      m20 m21 m22 m23
      m30 m31 m32 m33 ->
        let
          invDet = recip $
              s0 * c5
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
          rowMajor
            i00 i01 i02 i03
            i10 i11 i12 i13
            i20 i21 i22 i23
            i30 i31 i32 i33

foreign import ccall unsafe "Mat4xMat4_SIMD" m4m4simd :: ByteArray# -> ByteArray# -> ByteArray# -> IO ()

{-# INLINE matrixProduct #-}
matrixProduct :: Mat4 -> Mat4 -> Mat4
matrixProduct (Mat4 l) (Mat4 r) = unsafePerformIO do
  result@(Mat4 m) <- newMat4
  m4m4simd l r m
  pure result

foreign import ccall unsafe "Mat4xVec4_SIMD" m4v4simd :: ByteArray# -> ByteArray# -> ByteArray# -> IO ()

{- | Matrix - column vector multiplication (post)

@
vOut = p <> v <> m !* vIn
@
-}
(!*) :: Coercible a Mat4 => a -> Vec4 -> Vec4
(!*) (coerce -> Mat4 m) (Vec4 v) = unsafePerformIO do
  result@(Vec4 o) <- newVec4
  !() <- m4v4simd m v o
  pure result

infixr 5 !*

instance Semigroup Mat4 where
  {-# INLINE (<>) #-}
  (<>) = matrixProduct

instance Monoid Mat4 where
  {-# INLINE mempty #-}
  mempty = identity

instance Show Mat4 where
  show = showColumns

showColumns :: Mat4 -> String
showColumns cm = withColMajor cm
  \ e0 e4 e8 eC
    e1 e5 e9 eD
    e2 e6 eA eE
    e3 e7 eB eF ->
  unlines
    [ printf  "/ %.4f %.4f %.4f %.4f \\" e0 e4 e8 eC
    , printf  "| %.4f %.4f %.4f %.4f |"  e1 e5 e9 eD
    , printf  "| %.4f %.4f %.4f %.4f |"  e2 e6 eA eE
    , printf "\\ %.4f %.4f %.4f %.4f /"  e3 e7 eB eF
    ]

showRows :: Mat4 -> String
showRows rm = withRowMajor rm
  \ e0 e1 e2 e3
    e4 e5 e6 e7
    e8 e9 eA eB
    eC eD eE eF ->
  unlines
    [ printf "[ %.4f %.4f %.4f %.4f |" e0 e1 e2 e3
    , printf "| %.4f %.4f %.4f %.4f |" e4 e5 e6 e7
    , printf "| %.4f %.4f %.4f %.4f |" e8 e9 eA eB
    , printf "| %.4f %.4f %.4f %.4f ]" eC eD eE eF
    ]
