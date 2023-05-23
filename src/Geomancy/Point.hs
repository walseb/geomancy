{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

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

import Control.DeepSeq (NFData)
import Data.AffineSpace (AffineSpace)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GHC.Ix (Ix)
import qualified Data.AffineSpace as AffineSpace

import Geomancy.Elementwise (Elementwise(..))
import Geomancy.Gl.Block (Block(..))
import Geomancy.Vec2 (Vec2)
import Geomancy.Vec3 (Vec3, Packed)
import Geomancy.Vec4 (Vec4)
import Geomancy.Vector (VectorSpace(..))
import qualified Geomancy.Vector as Vector

newtype Point v = Point v
  deriving (Generic)
  deriving anyclass (Block)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Ix, NFData, Num, Fractional, MonoFunctor, MonoPointed, Elementwise, Storable)

type instance Element (Point v) = Element v

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
