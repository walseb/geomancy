{-# LANGUAGE FlexibleContexts #-}

module Geomancy.Vector
  ( VectorSpace(..)
  , (^*)
  , quadrance
  , lerp
  , lerpClip
  ) where

import Data.VectorSpace (VectorSpace(..))
import Geomancy.Interpolate (linear)

{-# INLINE (^*) #-}
(^*) :: VectorSpace v a => v -> a -> v
(^*) = flip (*^)

{-# INLINE quadrance #-}
quadrance :: VectorSpace v a => v -> a
quadrance v = dot v v

{-# INLINE lerp #-}
lerp :: (VectorSpace v a, Num a) => v -> v -> a -> v
lerp = linear

{-# INLINE lerpClip #-}
lerpClip :: (VectorSpace v a, Ord a, Num a) => v -> v -> a -> v
lerpClip a b t
  | t <= 0 = a
  | t >= 1 = b
  | otherwise = lerp a b t
