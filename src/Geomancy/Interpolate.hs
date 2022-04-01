{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Geomancy.Interpolate
  ( linearV
  , linearF
  , b1

  , quadraticV
  , quadraticF
  , b2

  , cubicV
  , cubicF
  , b3
  ) where

import Geomancy.Vector (VectorSpace, (^*), (^+^))

{-# INLINEABLE linearV #-}
linearV :: VectorSpace v a => v -> v -> a -> v
linearV p0 p1 t =
  p0 ^* b01 ^+^
  p1 ^* b11
  where
    (b01, b11) = b1 t

{-# INLINEABLE linearF #-}
linearF :: Float -> Float -> Float -> Float
linearF p0 p1 t =
  p0 ^* b01 ^+^
  p1 ^* b11
  where
    (b01, b11) = b1 t

{-# INLINE b1 #-}
b1 :: Num b => b -> (b, b)
b1 t =
  ( 1 - t
  , t
  )

{-# INLINEABLE quadraticV #-}
quadraticV :: VectorSpace v a => v -> v -> v -> a -> v
quadraticV p0 p1 p2 t =
  p0 ^* b02 ^+^
  p1 ^* b12 ^+^
  p2 ^* b22
  where
    (b02, b12, b22) = b2 t

{-# INLINEABLE quadraticF #-}
quadraticF :: Float -> Float -> Float -> Float -> Float
quadraticF p0 p1 p2 t =
  p0 * b02 +
  p1 * b12 +
  p2 * b22
  where
    (b02, b12, b22) = b2 t

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

{-# INLINEABLE cubicV #-}
cubicV :: VectorSpace v a => v -> v -> v -> v -> a -> v
cubicV p0 p1 p2 p3 t =
  p0 ^* b03 ^+^
  p1 ^* b13 ^+^
  p2 ^* b23 ^+^
  p3 ^* b33
  where
    (b03, b13, b23, b33) = b3 t

{-# INLINEABLE cubicF #-}
cubicF :: Float -> Float -> Float -> Float -> Float -> Float
cubicF p0 p1 p2 p3 t =
  p0 * b03 +
  p1 * b13 +
  p2 * b23 +
  p3 * b33
  where
    (b03, b13, b23, b33) = b3 t

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
