module Geomancy.Vector
  ( VectorSpace(..)
  , (^*)
  , quadrance
  , lerp
  , lerpClip
  ) where

import Data.VectorSpace (VectorSpace(..))

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
