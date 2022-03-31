{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Geomancy.Gl.Funs where

class GlClamp x val where
  glMin :: x -> val -> x
  glMax :: x -> val -> x
  glClamp :: x -> val -> val -> x

  glClamp x minVal = glMin (glMax x minVal)

glSaturate :: forall x . (GlClamp x x, Num x) => x -> x
glSaturate x = glClamp @_ @x x 0 1

instance GlClamp Float Float where
  glMin = min
  glMax = max

class GlClamp edge x => GlStep edge x where
  glStep :: edge -> x -> x
  glSmoothstep :: edge -> edge -> x -> x
  glSmootherstep :: edge -> edge -> x -> x

instance GlStep Float Float where
  {-# INLINE glStep #-}
  glStep edge x =
    if x < edge then
      0
    else
      1

  glSmoothstep edge0 edge1 x =
    glSmoothstepPoly . glSaturate $
      glNormRange edge0 edge1 x

  glSmootherstep edge0 edge1 x =
    glSmootherstepPoly . glSaturate $
      glNormRange edge0 edge1 x

{-# INLINE glNormRange #-}
glNormRange :: Fractional a => a -> a -> a -> a
glNormRange edge0 edge1 x = (x - edge0) / (edge1 - edge0)

{-# INLINE glSmoothstepPoly #-}
glSmoothstepPoly :: Num a => a -> a
glSmoothstepPoly t = t * t * (3 - 2 * t)

{-# INLINE glSmootherstepPoly #-}
glSmootherstepPoly :: Num a => a -> a
glSmootherstepPoly t = t * t * t * (t * (t * 6 - 15) + 10)

class GlNearest a where
  glCeil  :: a -> a
  glFloor :: a -> a
  glRound :: a -> a
  glTrunc :: a -> a

instance GlNearest Float where
  {-# INLINE glCeil #-}
  glCeil  = fromInteger . ceiling

  {-# INLINE glFloor #-}
  glFloor = fromInteger . floor

  {-# INLINE glRound #-}
  glRound = fromInteger . round

  {-# INLINE glTrunc #-}
  glTrunc = fromInteger . truncate

{-# INLINE glFract #-}
glFract :: (Num a, GlNearest a) => a -> a
glFract x = x - glFloor x

class GlModf i f where
  glModf :: f -> (i, f)

instance GlModf Integer Float where
  {-# INLINE glModf #-}
  glModf x =
    let
      integral = floor x
    in
      (integral, x - fromIntegral integral)

class GlMod x y where
  glMod :: x -> y -> x

instance GlMod Float Float where
  {-# INLINE glMod #-}
  glMod x y = x - y * glFloor (x / y)

class GlMix alpha x where
  glMix :: x -> x -> alpha -> x

instance GlMix Float Float where
  {-# INLINE glMix #-}
  glMix x y t = x * (1 - t) + y * t
