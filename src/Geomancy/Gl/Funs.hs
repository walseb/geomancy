{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Geomancy.Gl.Funs
  ( GlClamp(..)
  , glSaturate

  , GlStep(..)
  , normRange
  , smoothstepPoly
  , smootherstepPoly

  , GlNearest(..)
  , glFract

  , GlMod(..)
  , GlModf(..)

  , GlMix(..)
  ) where

import Geomancy.Elementwise (Element, Elementwise(..))
import Geomancy.Interpolate (linear, linearE)

class GlClamp edge a where
  glMin :: a -> edge -> a
  glMax :: a -> edge -> a
  glClamp :: a -> edge -> edge -> a

  glClamp x minVal = glMin (glMax x minVal)

instance {-# OVERLAPS #-} GlClamp Float Float where
  glMin = min
  glMax = max

instance (Element v ~ Float, Elementwise v) => GlClamp Float v where
  {-# INLINE glMin #-}
  glMin a b = emap2 glMin a (epoint b)

  {-# INLINE glMax #-}
  glMax a b = emap2 glMax a (epoint b)

instance (Element v ~ Float, Elementwise v) => GlClamp v v where
  {-# INLINE glMin #-}
  glMin = emap2 glMin

  {-# INLINE glMax #-}
  glMax = emap2 glMax

glSaturate :: forall a . (GlClamp a a, Num a) => a -> a
glSaturate x = glClamp @a x 0 1

class GlClamp edge a => GlStep edge a where
  glStep :: edge -> a -> a
  glSmoothstep :: edge -> edge -> a -> a
  glSmootherstep :: edge -> edge -> a -> a

instance (Element v ~ Float, Elementwise v) => GlStep Float v where
  glStep edge = emap2 glStep (epoint edge)
  glSmoothstep edge0 edge1 = emap3 glSmoothstep (epoint edge0) (epoint edge1)
  glSmootherstep edge0 edge1 = emap3 glSmootherstep (epoint edge0) (epoint edge1)

instance (Element v ~ Float, Elementwise v) => GlStep v v where
  glStep = emap2 glStep
  glSmoothstep = emap3 glSmoothstep
  glSmootherstep = emap3 glSmootherstep

instance {-# OVERLAPS #-} GlStep Float Float where
  {-# INLINE glStep #-}
  glStep edge x =
    if x < edge then
      0
    else
      1

  glSmoothstep edge0 edge1 x =
    smoothstepPoly . glSaturate $
      normRange edge0 edge1 x

  glSmootherstep edge0 edge1 x =
    smootherstepPoly . glSaturate $
      normRange edge0 edge1 x

{-# INLINE normRange #-}
normRange :: Fractional a => a -> a -> a -> a
normRange edge0 edge1 x = (x - edge0) / (edge1 - edge0)

{-# INLINE smoothstepPoly #-}
smoothstepPoly :: Num a => a -> a
smoothstepPoly t = t * t * (3 - 2 * t)

{-# INLINE smootherstepPoly #-}
smootherstepPoly :: Num a => a -> a
smootherstepPoly t = t * t * t * (t * (t * 6 - 15) + 10)

class GlNearest a where
  glCeil  :: a -> a
  glFloor :: a -> a
  glRound :: a -> a
  glTrunc :: a -> a

  default glCeil :: (Elementwise a, Element a ~ Float) => a -> a
  glCeil = emap glCeil

  default glFloor :: (Elementwise a, Element a ~ Float) => a -> a
  glFloor = emap glFloor

  default glRound :: (Elementwise a, Element a ~ Float) => a -> a
  glRound = emap glRound

  default glTrunc :: (Elementwise a, Element a ~ Float) => a -> a
  glTrunc = emap glTrunc

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

instance GlModf Float Float where
  {-# INLINE glModf #-}
  glModf x = (fromInteger i, f)
    where
      (i, f) = glModf x

class GlMod x y where
  glMod :: x -> y -> x

instance GlMod Float Float where
  {-# INLINE glMod #-}
  glMod x y = x - y * glFloor (x / y)

class GlMix alpha x where
  glMix :: x -> x -> alpha -> x

instance {-# OVERLAPS #-} GlMix Float Float where
  {-# INLINE glMix #-}
  glMix = linear

instance (Element v ~ Float, Elementwise v) => GlMix Float v where
  {-# INLINE glMix #-}
  glMix a b t = linearE a b (epoint t)

instance (Element v ~ Float, Elementwise v) => GlMix v v where
  {-# INLINE glMix #-}
  glMix = linearE
