{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Float@.

module Geomancy.Vec2
  ( Vec2
  , vec2
  , withVec2
  , pattern WithVec2
  , fromTuple

  , (^*)
  , (^/)
  , lerp

  , dot
  , normalize
  ) where

import Control.DeepSeq (NFData(rnf))
import Foreign (Storable(..))

data Vec2 = Vec2
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  deriving (Eq, Ord, Show)

{-# INLINE vec2 #-}
vec2 :: Float -> Float -> Vec2
vec2 = Vec2

{-# INLINE withVec2 #-}
withVec2
  :: Vec2
  -> (Float -> Float -> r)
  -> r
withVec2 (Vec2 a b) f = f a b

pattern WithVec2 :: Float -> Float -> Vec2
pattern WithVec2 a b <- ((`withVec2` (,)) -> (a, b))
{-# COMPLETE WithVec2 #-}

{-# INLINE fromTuple #-}
fromTuple :: (Float, Float) -> Vec2
fromTuple (x, y) = vec2 x y

instance NFData Vec2 where
  rnf Vec2{} = ()

instance Num Vec2 where
  {-# INLINE (+) #-}
  Vec2 l1 l2 + Vec2 r1 r2 =
    Vec2
      (l1 + r1)
      (l2 + r2)

  {-# INLINE (-) #-}
  Vec2 l1 l2 - Vec2 r1 r2 =
    Vec2
      (l1 - r1)
      (l2 - r2)

  {-# INLINE (*) #-}
  Vec2 l1 l2 * Vec2 r1 r2 =
    Vec2
      (l1 * r1)
      (l2 * r2)

  {-# INLINE abs #-}
  abs (Vec2 a b) =
    Vec2 (abs a) (abs b)

  {-# INLINE signum #-}
  signum (Vec2 a b) =
    Vec2 (signum a) (signum b)

  {-# INLINE fromInteger #-}
  fromInteger x = Vec2 x' x'
    where
      x' = fromInteger x

instance Fractional Vec2 where
  {-# INLINE (/) #-}
  Vec2 l1 l2 / Vec2 r1 r2 =
    Vec2 (l1 / r1) (l2 / r2)

  {-# INLINE recip #-}
  recip (Vec2 a b) =
    Vec2 (recip a) (recip b)

  {-# INLINE fromRational #-}
  fromRational x = Vec2 x' x'
    where
      x' = fromRational x

{-# INLINE (^*) #-}
(^*) :: Vec2 -> Float -> Vec2
Vec2 a b ^* x =
  Vec2
    (a * x)
    (b * x)

{-# INLINE (^/) #-}
(^/) :: Vec2 -> Float -> Vec2
Vec2 a b ^/ x =
  Vec2
    (a / x)
    (b / x)

{-# INLINE lerp #-}
lerp :: Float -> Vec2 -> Vec2 -> Vec2
lerp alpha u v = u ^* alpha + v ^* (1 - alpha)

{-# INLINE dot #-}
dot :: Vec2 -> Vec2 -> Float
dot (Vec2 l1 l2) (Vec2 r1 r2) =
  l1 * r1 + l2 * r2

{-# INLINE normalize #-}
normalize :: Vec2 -> Vec2
normalize v =
  if nearZero q || nearZero (1 - q) then
    v
  else
    let
      Vec2 x y = v
    in
      Vec2 (x / l) (y / l)

  where
    q = dot v v
    l = sqrt q

    nearZero a = abs a <= 1e-6

instance Storable Vec2 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 8

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v4 =
    withVec2 v4 \a b -> do
      pokeByteOff ptr 0 a
      pokeByteOff ptr 4 b

  {-# INLINE peek #-}
  peek ptr = vec2
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
