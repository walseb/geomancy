{-# LANGUAGE BlockArguments #-}

-- | Specialized and inlined @V4 Float@.

module Geomancy.Vec4
  ( Vec4
  , vec4
  , withVec4

  , (^*)
  , lerp

  , dot
  , normalize
  ) where

import Foreign (Storable(..), castPtr)

data Vec4 = Vec4
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  deriving (Eq, Ord, Show)

{-# INLINE vec4 #-}
vec4 :: Float -> Float -> Float -> Float -> Vec4
vec4 = Vec4

{-# INLINE withVec4 #-}
withVec4
  :: Vec4
  -> (Float -> Float -> Float -> Float -> r)
  -> r
withVec4 (Vec4 a b c d) f = f a b c d

instance Num Vec4 where
  {-# INLINE (+) #-}
  Vec4 l1 l2 l3 l4 + Vec4 r1 r2 r3 r4 =
    Vec4
      (l1 + r1)
      (l2 + r2)
      (l3 + r3)
      (l4 + r4)

  {-# INLINE (-) #-}
  Vec4 l1 l2 l3 l4 - Vec4 r1 r2 r3 r4 =
    Vec4
      (l1 - r1)
      (l2 - r2)
      (l3 - r3)
      (l4 - r4)

  {-# INLINE (*) #-}
  Vec4 l1 l2 l3 l4 * Vec4 r1 r2 r3 r4 =
    Vec4
      (l1 * r1)
      (l2 * r2)
      (l3 * r3)
      (l4 * r4)

  {-# INLINE abs #-}
  abs (Vec4 a b c d) =
    Vec4 (abs a) (abs b) (abs c) (abs d)

  {-# INLINE signum #-}
  signum (Vec4 a b c d) =
    Vec4 (signum a) (signum b) (signum c) (signum d)

  {-# INLINE fromInteger #-}
  fromInteger x = Vec4 x' x' x' x'
    where
      x' = fromInteger x

{-# INLINE (^*) #-}
(^*) :: Vec4 -> Float -> Vec4
Vec4 a b c d ^* x =
  Vec4
    (a * x)
    (b * x)
    (c * x)
    (d * x)

{-# INLINE lerp #-}
lerp :: Float -> Vec4 -> Vec4 -> Vec4
lerp alpha u v = u ^* alpha + v ^* (1 - alpha)

{-# INLINE dot #-}
dot :: Vec4 -> Vec4 -> Float
dot (Vec4 l1 l2 l3 l4) (Vec4 r1 r2 r3 r4) =
  l1 * r1 +
  l2 * r2 +
  l3 * r3 +
  l4 * r4

{-# INLINE normalize #-}
normalize :: Vec4 -> Vec4
normalize v =
  if nearZero q || nearZero (1-q) then
    v
  else
    let
      Vec4 x y z w = v
    in
      Vec4 (x / l) (y / l) (z / l) (w / l)

  where
    q = dot v v
    l = sqrt q

    nearZero a = abs a <= 1e-6

instance Storable Vec4 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 16

  {-# INLINE poke #-}
  poke ptr v4 =
    withVec4 v4 \a b c d -> do
      poke ptr' a
      pokeElemOff ptr' 1 b
      pokeElemOff ptr' 2 c
      pokeElemOff ptr' 3 d
    where
      ptr' = castPtr ptr

  {-# INLINE peek #-}
  peek ptr = vec4
    <$> peek ptr'
    <*> peekElemOff ptr' 1
    <*> peekElemOff ptr' 2
    <*> peekElemOff ptr' 3
    where
      ptr' = castPtr ptr
