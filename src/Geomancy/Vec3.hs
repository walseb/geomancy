{-# LANGUAGE BlockArguments #-}

-- | Specialized and inlined @V3 Float@.

module Geomancy.Vec3
  ( Vec3
  , vec3
  , withVec3

  , (^*)
  , (^/)
  , lerp

  , cross
  , dot
  , normalize
  ) where

import Control.DeepSeq (NFData(rnf))
import Foreign (Storable(..), castPtr)

data Vec3 = Vec3
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  deriving (Eq, Ord, Show)

{-# INLINE vec3 #-}
vec3 :: Float -> Float -> Float -> Vec3
vec3 = Vec3

{-# INLINE withVec3 #-}
withVec3
  :: Vec3
  -> (Float -> Float -> Float -> r)
  -> r
withVec3 (Vec3 a b c) f = f a b c

instance NFData Vec3 where
  rnf Vec3{} = ()

instance Num Vec3 where
  {-# INLINE (+) #-}
  Vec3 a b c + Vec3 d e f =
    Vec3
      (a + d)
      (b + e)
      (c + f)

  {-# INLINE (-) #-}
  Vec3 a b c - Vec3 d e f =
    Vec3
      (a - d)
      (b - e)
      (c - f)

  {-# INLINE (*) #-}
  Vec3 a b c * Vec3 d e f =
    Vec3
      (a * d)
      (b * e)
      (c * f)

  {-# INLINE abs #-}
  abs (Vec3 a b c) =
    Vec3 (abs a) (abs b) (abs c)

  {-# INLINE signum #-}
  signum (Vec3 a b c) =
    Vec3 (signum a) (signum b) (signum c)

  {-# INLINE fromInteger #-}
  fromInteger x = Vec3 x' x' x'
    where
      x' = fromInteger x

{-# INLINE (^*) #-}
(^*) :: Vec3 -> Float -> Vec3
Vec3 a b c ^* x =
  Vec3
    (a * x)
    (b * x)
    (c * x)

{-# INLINE (^/) #-}
(^/) :: Vec3 -> Float -> Vec3
Vec3 a b c ^/ x =
  Vec3
    (a / x)
    (b / x)
    (c / x)

{-# INLINE lerp #-}
lerp :: Float -> Vec3 -> Vec3 -> Vec3
lerp alpha u v = u ^* alpha + v ^* (1 - alpha)

{-# INLINE cross #-}
cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 d e f) =
  Vec3
    (b * f - c * e)
    (c * d - a * f)
    (a * e - b * d)

{-# INLINE dot #-}
dot :: Vec3 -> Vec3 -> Float
dot (Vec3 a b c) (Vec3 d e f) =
  a * d +
  b * e +
  c * f

{-# INLINE normalize #-}
normalize :: Vec3 -> Vec3
normalize v =
  if nearZero q || nearZero (1-q) then
    v
  else
    let
      Vec3 x y z = v
    in
      Vec3 (x / l) (y / l) (z / l)

  where
    q = dot v v
    l = sqrt q

    nearZero a = abs a <= 1e-6

-- XXX: GPU layouts call for some padding.
instance Storable Vec3 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 16

  {-# INLINE alignment #-}
  alignment _ = 16

  {-# INLINE poke #-}
  poke ptr v3 =
    withVec3 v3 \a b c -> do
      poke ptr' a
      pokeElemOff ptr' 1 b
      pokeElemOff ptr' 2 c
    where
      ptr' = castPtr ptr

  {-# INLINE peek #-}
  peek ptr =
    vec3 <$> peek ptr' <*> peekElemOff ptr' 1 <*> peekElemOff ptr' 2
    where
      ptr' = castPtr ptr
