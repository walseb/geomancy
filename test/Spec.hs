{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import GHC.Stack (withFrozenCallStack)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import qualified Foreign
import qualified Linear
import qualified Geomancy
import qualified Geomancy.Mat4

import Linear ((!*!))

main :: IO ()
main = do
  passed <- checkParallel discovered
  if passed then
    exitSuccess
  else
    exitFailure

-- | Enough tests to cover the principles
pattern PROP_TESTS :: TestLimit
pattern PROP_TESTS = 10_000

-- | Try harder to catch FP precision errors
pattern PROP_TESTS_BRUTAL :: TestLimit
pattern PROP_TESTS_BRUTAL = 10_000_000

prop_assoc_multiply :: Property
prop_assoc_multiply = withTests PROP_TESTS $ property do
  (a, a_) <- forAllTransform
  (b, b_) <- forAllTransform
  (c, c_) <- forAllTransform

  let
    ab'c = (a <> b) <> c
    a'bc = a <> (b <> c)
    delta' = nearlyEqualMat4 ab'c a'bc
  annotateShow delta'

  let
    ab_c = (a_ !*! b_) !*! c_
    a_bc = a_ !*! (b_ !*! c_)
    delta_ = nearlyEqualM44 ab_c a_bc
  annotateShow delta_

  -- Intra-library transitivity
  unless (null $ catMaybes delta') do
    -- XXX: check only if there is some outstanding error
    delta' === delta_

  -- Inter-library calculated values nearlyEqual
  ab'c_ <- toM44 ab'c
  catMaybes (nearlyEqualM44 ab_c ab'c_) === []

  a'bc_ <- toM44 a'bc
  catMaybes (nearlyEqualM44 a_bc a'bc_) === []

forAllTransform :: PropertyT IO (Geomancy.Mat4.Mat4, Linear.M44 Float)
forAllTransform = withFrozenCallStack do
  (_name, g) <- forAllWith fst genTransform
  l <- toM44 g
  pure (g, l)

genTransform :: Gen ([Char], Geomancy.Mat4)
genTransform = Gen.choice
  [ genIdentity
  , genTranslate
  , genRotate
  , genScale
  ]
  where
    genIdentity = pure ("identity", mempty)

    genTranslate = do
      x <- Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
      y <- Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
      z <- Gen.float (Range.linearFracFrom 0.0 (-1e6) 1e6)
      pure
        ( printf "translate %0.4f %0.4f %0.4f" x y z
        , Geomancy.Mat4.translate x y z
        )

    genRotate = do
      (name, axis) <- Gen.element
        [ ("rotate/x", Geomancy.Mat4.rotateX)
        , ("rotate/y", Geomancy.Mat4.rotateY)
        , ("rotate/z", Geomancy.Mat4.rotateZ)
        ]
      angle <- Gen.float (Range.linearFracFrom 0.0 (-4 * pi) (4 * pi))
      pure
        ( printf "%s %0.4f" name angle
        , axis angle
        )

    genScale = do
      x <- Gen.float (Range.linearFracFrom 1.0 1e-6 1e6)
      y <- Gen.float (Range.linearFracFrom 1.0 1e-6 1e6)
      z <- Gen.float (Range.linearFracFrom 1.0 1e-6 1e6)
      pure
        ( printf "scale %0.4f %0.4f %0.4f" x y z
        , Geomancy.Mat4.scale x y z
        )

-- toVulkan :: Linear.M44 Float -> Linear.M44 Float
-- toVulkan = {- Linear.transpose . -} (correction Linear.!*!)
--   where
--     correction = Linear.V4
--       (Linear.V4 1   0  0   0)
--       (Linear.V4 0 (-1) 0   0)
--       (Linear.V4 0   0  0.5 0.5)
--       (Linear.V4 0   0  0   1)

toM44 :: MonadIO io => Geomancy.Mat4 -> io (Linear.M44 Float)
toM44 mat4 =
  liftIO $
    Foreign.with mat4 $
      Foreign.peek . Foreign.castPtr

type NearlyEqual = Maybe (Float, Float, Float)

nearlyEqualMat4 :: Geomancy.Mat4.Mat4 -> Geomancy.Mat4.Mat4 -> [NearlyEqual]
nearlyEqualMat4 a b = Geomancy.Mat4.zipWith nearlyEqual a b

nearlyEqualM44 :: Linear.M44 Float -> Linear.M44 Float -> [NearlyEqual]
nearlyEqualM44 a b = zipWith nearlyEqual (concatMap toList a) (concatMap toList b)

nearlyEqual :: Float -> Float -> NearlyEqual
nearlyEqual x y =
  if x == y || abs (1 - x / y) < 0.001 then
    Nothing
  else
    Just (x, y, abs (1 - x / y))

discovered :: Group
discovered = $$(discover)
