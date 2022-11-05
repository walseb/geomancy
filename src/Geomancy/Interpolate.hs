{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Geomancy.Interpolate
  ( linear
  , linearE
  , b1

  , quadratic
  , quadraticE
  , b2

  , cubic
  , cubicE
  , b3
  ) where

import Data.VectorSpace (VectorSpace, (*^), (^+^))
import Geomancy.Elementwise (Element, Elementwise(..))

{-# INLINEABLE linear #-}
linear :: (VectorSpace v a, Num a) => v -> v -> a -> v
linear p0 p1 t =
  b01 *^ p0 ^+^
  b11 *^ p1
  where
    (b01, b11) = b1 t

{-# INLINEABLE linearE #-}
linearE :: (Elementwise v, Element v ~ Float) => v -> v -> v -> v
linearE = emap3 linear

{-# INLINE b1 #-}
b1 :: Num b => b -> (b, b)
b1 t =
  ( 1 - t
  , t
  )

{-# INLINEABLE quadratic #-}
quadratic :: (VectorSpace v a, Num a) => v -> v -> v -> a -> v
quadratic p0 p1 p2 t =
  b02 *^ p0 ^+^
  b12 *^ p1 ^+^
  b22 *^ p2
  where
    (b02, b12, b22) = b2 t

{-# INLINEABLE quadraticE #-}
quadraticE :: (Elementwise v, Element v ~ Float) => v -> v -> v -> v -> v
quadraticE = emap4 quadratic

{-# INLINE b2 #-}
b2 :: Num c => c -> (c, c, c)
b2 t =
  ( 1 -
    2 * t +
    t * t

  , 2 * t -
    2 * t * t

  , t * t
  )

{-# INLINEABLE cubic #-}
cubic :: (VectorSpace v a, Num a) => v -> v -> v -> v -> a -> v
cubic p0 p1 p2 p3 t =
  b03 *^ p0 ^+^
  b13 *^ p1 ^+^
  b23 *^ p2 ^+^
  b33 *^ p3
  where
    (b03, b13, b23, b33) = b3 t

{-# INLINEABLE cubicE #-}
cubicE :: (Elementwise v, Element v ~ Float) => v -> v -> v -> v -> v -> v
cubicE = emap5 cubic

{-# INLINE b3 #-}
b3 :: Num d => d -> (d, d, d, d)
b3 t =
  ( 1 - 3 * t +
    3 * t * t -
    t * t * t

  , 3 * t -
    6 * t * t +
    3 * t * t * t

  , 3 * t * t -
    3 * t * t * t

  , t * t * t
  )
