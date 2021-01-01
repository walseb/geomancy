{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main

import Data.List (foldl1')
import qualified Foreign

import qualified Geomancy
import qualified Geomancy.Mat4

import qualified Linear
import qualified Linear.Matrix

main :: IO ()
main = defaultMain
  [ bgroup "4x4 transpose"
      mat4transpose

  , bgroup "4x4 multiply" $ flip map [2, 3, 10, 100, 1000] \size ->
      env (clones size mempty Linear.identity) $
        mat4multiply size

  , bgroup "4x4 poke" $ flip map [10, 100, 1000, 10000] \size ->
      env (clones size mempty Linear.identity) $
        mat4poke size
  ]

mat4transpose :: [Benchmark]
mat4transpose =
  [ bench "geomancy" $ whnf Geomancy.Mat4.transpose mempty
  , bench "linear"   $ whnf Linear.Matrix.transpose (Linear.identity :: Linear.M44 Float)
  ]

mat4multiply
  :: Int
  -> ([Geomancy.Mat4], [Linear.M44 Float])
  -> Benchmark
mat4multiply size ~(mat4s, m44s) = bgroup (show size)
  [ bench "geomancy" $ whnf multGeomancy mat4s
  , bench "linear"   $ whnf multLinear m44s
  ]
  where
    multGeomancy = foldl1' (<>)
    multLinear = foldl1' (Linear.!*!)

clones :: Applicative f => Int -> a -> b -> f ([a], [b])
clones n geo lin = (,) <$> genMat4s <*> genM44s
  where
    genMat4s = pure $! replicate n geo
    genM44s = pure $! replicate n lin

mat4poke
  :: Int
  -> ([Geomancy.Mat4], [Linear.M44 Float])
  -> Benchmark
mat4poke size ~(mat4s, m44s) = bgroup (show size)
  [ bench "geomancy" $ nfIO pokeGeomancy --  mat4s
  , bench "linear"   $ nfIO pokeLinear --  m44s
  , bench "linear/T" $ nfIO pokeLinearTranspose
  ]
  where
    pokeGeomancy =
      Foreign.withArray mat4s \_ptr ->
        pure ()

    pokeLinear =
      Foreign.withArray m44s \_ptr ->
        pure ()

    pokeLinearTranspose =
      Foreign.withArray (map Linear.transpose m44s) \_ptr ->
        pure ()
