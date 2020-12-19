{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function (on)
import Data.Bifunctor (bimap)
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

import qualified Foreign
import qualified Linear
import qualified Geomancy
import qualified Geomancy.Mat4
import qualified Geomancy.Vulkan.Projection

prop_multiply :: Property
prop_multiply = withTests 10_000 $ property do
  (nameA, a) <- forAll genTransform
  annotate nameA

  (nameB, b) <- forAll genTransform
  annotate nameB

  let transMult = Geomancy.Mat4.transpose (a <> b)
  annotateShow transMult

  let multTrans = Geomancy.Mat4.transpose a <> Geomancy.Mat4.transpose b
  annotateShow multTrans

  let
    delta =
      Geomancy.Mat4.elementwise transMult multTrans \a b ->
        abs (a - b)
  annotateShow delta

  aLinear <- toM44 a
  bLinear <- toM44 b
  let
    mtLinear = Linear.transpose $ aLinear Linear.!*! bLinear
    tmLinear = Linear.transpose aLinear Linear.!*! Linear.transpose bLinear
    deltaLinear = abs $ mtLinear - tmLinear
  delta' <- toM44 delta
  delta' === deltaLinear

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

toVulkan :: Linear.M44 Float -> Linear.M44 Float
toVulkan = {- Linear.transpose . -} (correction Linear.!*!)
  where
    correction = Linear.V4
      (Linear.V4 1   0  0   0)
      (Linear.V4 0 (-1) 0   0)
      (Linear.V4 0   0  0.5 0.5)
      (Linear.V4 0   0  0   1)

toM44 :: MonadIO io => Geomancy.Mat4 -> io (Linear.M44 Float)
toM44 mat4 =
  liftIO $
    Foreign.with mat4 $
      Foreign.peek . Foreign.castPtr

main :: IO ()
main = do
  passed <- checkParallel $$(discover)
  if passed then
    exitSuccess
  else
    exitFailure
