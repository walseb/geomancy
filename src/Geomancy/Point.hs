{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Geomancy.Point
  ( Point(..)

  , Point2
  , Point3
  , Point3P
  , Point4

  , AffineSpace
  , (AffineSpace..+^)
  , (AffineSpace..-^)
  , (AffineSpace..-.)

  , qd
  , distance
  , lerp
  ) where

import Foreign.Storable (Storable)
import Control.DeepSeq (NFData)
import Data.AffineSpace (AffineSpace)
import qualified Data.AffineSpace as AffineSpace

import Geomancy.Vec2 (Vec2)
import Geomancy.Vec3 (Vec3, Packed)
import Geomancy.Vec4 (Vec4)
import Geomancy.Vector (VectorSpace(..))
import qualified Geomancy.Vector as Vector

newtype Point v = Point v
  deriving (Eq, Ord, Show, Storable, NFData)

type Point2 = Point Vec2
type Point3 = Point Vec3
type Point3P = Point Packed
type Point4 = Point Vec4

instance VectorSpace v Float => AffineSpace (Point v) v Float where
  origin = Point zeroVector

  {-# INLINE (.+^) #-}
  Point p .+^ v = Point (p ^+^ v)

  {-# INLINE (.-^) #-}
  Point p .-^ v = Point (p ^-^ v)

  {-# INLINE (.-.) #-}
  Point a .-. Point b = a ^-^ b

{-# INLINE qd #-}
qd :: VectorSpace v Float => Point v -> Point v -> Float
qd a b = Vector.quadrance (a AffineSpace..-. b)

{-# INLINE distance #-}
distance :: VectorSpace v Float => Point v -> Point v -> Float
distance a b = sqrt (qd a b)

{-# INLINE lerp #-}
lerp :: VectorSpace v Float => Point v -> Point v -> Float -> Point v
lerp (Point a) (Point b) = Point . Vector.lerp a b
