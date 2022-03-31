{-# LANGUAGE FlexibleContexts #-}

module Geomancy.Vector
  ( VectorSpace(..)
  , (^*)
  , quadrance
  , lerp
  , lerpClip

  , Geomancy.Vector.floor
  , fract
  , clamp
  , clamp01
  ) where

import Data.VectorSpace (VectorSpace(..))
import Data.MonoTraversable (Element, MonoFunctor(omap))

{-# INLINE (^*) #-}
(^*) :: VectorSpace v a => v -> a -> v
(^*) = flip (*^)

{-# INLINE quadrance #-}
quadrance :: VectorSpace v a => v -> a
quadrance v = dot v v

{-# INLINE lerp #-}
lerp :: VectorSpace v a => v -> v -> a -> v
lerp a b t = a ^* t ^+^ b ^* (1 - t)

{-# INLINE lerpClip #-}
lerpClip :: (VectorSpace v a, Ord a) => v -> v -> a -> v
lerpClip a b t
  | t <= 0 = a
  | t >= 1 = b
  | otherwise = lerp a b t

{-# INLINE floor #-}
floor :: (MonoFunctor v, RealFrac (Element v)) => v -> v
floor = omap (fromInteger . Prelude.floor)

{-# INLINE fract #-}
fract :: (VectorSpace v a, MonoFunctor v, RealFrac (Element v)) => v -> v
fract v = v ^-^ Geomancy.Vector.floor v

{-# INLINE clamp #-}
clamp
  :: (MonoFunctor mono, Ord (Element mono))
  => mono -> Element mono -> Element mono -> mono
clamp v a b = omap (\x -> min (max x a) b) v

{-# INLINE clamp01 #-}
clamp01
  :: (MonoFunctor mono, Ord (Element mono), Num (Element mono))
  => mono -> mono
clamp01 v = clamp v 0 1
