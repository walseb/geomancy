-- | Specialized and inlined @V3 Float@.

module Geomancy.Vec3 where

data Vec3 = Vec3
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  {-# UNPACK #-} !Float
  deriving (Eq, Ord, Show)

{-# INLINE (^-^) #-}
(^-^) :: Vec3 -> Vec3 -> Vec3
Vec3 a b c ^-^ Vec3 d e f =
  Vec3
    (a - d)
    (b - e)
    (c - f)

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
