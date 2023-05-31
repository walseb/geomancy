{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Float@.

module Geomancy.Vec2
  ( Vec2
  , vec2
  , withVec2
  , pattern WithVec2
  , fromTuple
  , convert

  , (^*)
  , (^/)
  , lerp

  , dot
  , normalize
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Coerce (Coercible, coerce)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Data.VectorSpace (VectorSpace)
import Foreign (Storable(..))
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)
import qualified Data.VectorSpace as VectorSpace

import Geomancy.Elementwise (Elementwise(..))
import Graphics.Gl.Block (Block(..))
import Geomancy.Gl.Funs (GlModf(..), GlNearest)

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

{-# INLINE convert #-}
convert :: Coercible v Vec2 => (Float -> a) -> (a -> a -> r) -> v -> r
convert f t v =
  withVec2 (coerce v) \a b ->
    t (f a) (f b)

pattern WithVec2 :: Float -> Float -> Vec2
pattern WithVec2 a b <- ((`withVec2` (,)) -> (a, b))
{-# COMPLETE WithVec2 #-}

{-# INLINE fromTuple #-}
fromTuple :: (Float, Float) -> Vec2
fromTuple (x, y) = vec2 x y

instance NFData Vec2 where
  rnf Vec2{} = ()

type instance Element Vec2 = Float

instance MonoFunctor Vec2 where
  {-# INLINE omap #-}
  omap f v =
    withVec2 v \x y ->
      vec2 (f x) (f y)

instance MonoPointed Vec2 where
  opoint x = vec2 x x

instance Elementwise Vec2 where
  {-# INLINE emap2 #-}
  emap2 f p0 p1 =
    withVec2 p0 \x0 y0 ->
    withVec2 p1 \x1 y1 ->
      vec2
        (f x0 x1)
        (f y0 y1)

  {-# INLINE emap3 #-}
  emap3 f p0 p1 p2 =
    withVec2 p0 \x0 y0 ->
    withVec2 p1 \x1 y1 ->
    withVec2 p2 \x2 y2 ->
      vec2
        (f x0 x1 x2)
        (f y0 y1 y2)

  {-# INLINE emap4 #-}
  emap4 f p0 p1 p2 p3 =
    withVec2 p0 \x0 y0 ->
    withVec2 p1 \x1 y1 ->
    withVec2 p2 \x2 y2 ->
    withVec2 p3 \x3 y3 ->
      vec2
        (f x0 x1 x2 x3)
        (f y0 y1 y2 y3)

  {-# INLINE emap5 #-}
  emap5 f p0 p1 p2 p3 p4 =
    withVec2 p0 \x0 y0 ->
    withVec2 p1 \x1 y1 ->
    withVec2 p2 \x2 y2 ->
    withVec2 p3 \x3 y3 ->
    withVec2 p4 \x4 y4 ->
      vec2
        (f x0 x1 x2 x3 x4)
        (f y0 y1 y2 y3 y4)

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

instance Floating Vec2 where
  pi = opoint pi

  exp = omap exp
  log = omap log
  sqrt = omap sqrt
  sin = omap sin
  cos = omap cos
  asin = omap asin
  acos = omap acos
  atan = omap atan
  sinh = omap sinh
  cosh = omap cosh
  asinh = omap asinh
  acosh = omap acosh
  atanh = omap atanh

  (**) = emap2 (**)

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

instance Block Vec2 where
  type PackedSize Vec2 = 8
  alignment140 _  = 8
  sizeOf140 _     = 8
  alignment430    = alignment140
  sizeOf430       = sizeOf140
  isStruct _      = False
  read140     = peekDiffOff
  write140    = pokeDiffOff
  read430     = read140
  write430    = write140
  readPacked  = read140
  writePacked = write140
  {-# INLINE alignment140 #-}
  {-# INLINE sizeOf140 #-}
  {-# INLINE alignment430 #-}
  {-# INLINE sizeOf430 #-}
  {-# INLINE isStruct #-}
  {-# INLINE read140 #-}
  {-# INLINE write140 #-}
  {-# INLINE read430 #-}
  {-# INLINE write430 #-}
  {-# INLINE readPacked #-}
  {-# INLINE writePacked #-}

instance VectorSpace Vec2 Float where
  zeroVector = epoint 0

  {-# INLINE (*^) #-}
  a *^ v = v Geomancy.Vec2.^* a

  {-# INLINE (^/) #-}
  v ^/ a = v Geomancy.Vec2.^/ a

  {-# INLINE (^+^) #-}
  (^+^) = emap2 (+)

  {-# INLINE (^-^) #-}
  (^-^) = emap2 (-)

  {-# INLINE negateVector #-}
  negateVector = emap negate

  {-# INLINE dot #-}
  dot = Geomancy.Vec2.dot

  {-# INLINE normalize #-}
  normalize = Geomancy.Vec2.normalize

instance GlNearest Vec2

instance GlModf Vec2 Vec2 where
  glModf v =
    withVec2 v \vx vy ->
      let
        (xi, xf) = glModf vx
        (yi, yf) = glModf vy
      in
        ( vec2 (fromInteger xi) (fromInteger yi)
        , vec2 xf yf
        )
