{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

#ifdef TH_LIFT
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | General matrix storage and operations.

module Geomancy.Mat4.Internal
  ( Mat4(..)
  , fromMemory
  , withMemory
  , newMat4
    -- * Order-independent operations
  , identity
  , transpose
  , pointwise
  , scalarMultiply
    -- * List operations in memory order
  , toListMemory
  , toList2dMemory
  , toListTrans
  , toList2dTrans
  , zipWith
  ) where

import Prelude hiding (zipWith)
import GHC.Exts hiding (VecCount(..), toList)

#ifdef TH_LIFT
import Language.Haskell.TH.Syntax (Lift(..))
#endif

import Control.DeepSeq (NFData(rnf))
import Foreign (Storable(..))
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)
import GHC.IO (IO(..))

import qualified Data.List as List

import Graphics.Gl.Block (Block(..))

data Mat4 = Mat4 ByteArray#

instance NFData Mat4 where
  rnf Mat4{} = ()

{- | Construct 'Mat4' from elements in memory order.
-}
{-# INLINE fromMemory #-}
fromMemory
  :: Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Float -> Float -> Float -> Float
  -> Mat4
fromMemory
  (F# e0) (F# e1) (F# e2) (F# e3)
  (F# e4) (F# e5) (F# e6) (F# e7)
  (F# e8) (F# e9) (F# ea) (F# eb)
  (F# ec) (F# ed) (F# ee) (F# ef) =
  runRW# \world ->
    let
      !(# world_, arr #) = newAlignedPinnedByteArray# 64# 16# world

      world0 = writeFloatArray# arr 0x0# e0 world_
      world1 = writeFloatArray# arr 0x1# e1 world0
      world2 = writeFloatArray# arr 0x2# e2 world1
      world3 = writeFloatArray# arr 0x3# e3 world2

      world4 = writeFloatArray# arr 0x4# e4 world3
      world5 = writeFloatArray# arr 0x5# e5 world4
      world6 = writeFloatArray# arr 0x6# e6 world5
      world7 = writeFloatArray# arr 0x7# e7 world6

      world8 = writeFloatArray# arr 0x8# e8 world7
      world9 = writeFloatArray# arr 0x9# e9 world8
      worldA = writeFloatArray# arr 0xA# ea world9
      worldB = writeFloatArray# arr 0xB# eb worldA

      worldC = writeFloatArray# arr 0xC# ec worldB
      worldD = writeFloatArray# arr 0xD# ed worldC
      worldE = writeFloatArray# arr 0xE# ee worldD
      worldF = writeFloatArray# arr 0xF# ef worldE

      !(# _world', arr' #) = unsafeFreezeByteArray# arr worldF
    in
      Mat4 arr'

{- | Reduce 'Mat4' with a function with @memory@ notation for arguments.
-}
{-# INLINE withMemory #-}
withMemory
  :: Mat4
  ->
    ( Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      Float -> Float -> Float -> Float ->
      r
    )
  -> r
withMemory (Mat4 arr) f =
  f
    (F# (indexFloatArray# arr 0x0#))
    (F# (indexFloatArray# arr 0x1#))
    (F# (indexFloatArray# arr 0x2#))
    (F# (indexFloatArray# arr 0x3#))

    (F# (indexFloatArray# arr 0x4#))
    (F# (indexFloatArray# arr 0x5#))
    (F# (indexFloatArray# arr 0x6#))
    (F# (indexFloatArray# arr 0x7#))

    (F# (indexFloatArray# arr 0x8#))
    (F# (indexFloatArray# arr 0x9#))
    (F# (indexFloatArray# arr 0xA#))
    (F# (indexFloatArray# arr 0xB#))

    (F# (indexFloatArray# arr 0xC#))
    (F# (indexFloatArray# arr 0xD#))
    (F# (indexFloatArray# arr 0xE#))
    (F# (indexFloatArray# arr 0xF#))

#ifdef TH_LIFT
instance Lift Mat4 where
  lift m = withMemory m
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
      [|
        fromMemory
          $(lift e0) $(lift e1) $(lift e2) $(lift e3)
          $(lift e4) $(lift e5) $(lift e6) $(lift e7)
          $(lift e8) $(lift e9) $(lift eA) $(lift eB)
          $(lift eC) $(lift eD) $(lift eE) $(lift eF)
      |]
  liftTyped m = withMemory m
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
      [||
        fromMemory
          $$(liftTyped e0) $$(liftTyped e1) $$(liftTyped e2) $$(liftTyped e3)
          $$(liftTyped e4) $$(liftTyped e5) $$(liftTyped e6) $$(liftTyped e7)
          $$(liftTyped e8) $$(liftTyped e9) $$(liftTyped eA) $$(liftTyped eB)
          $$(liftTyped eC) $$(liftTyped eD) $$(liftTyped eE) $$(liftTyped eF)
      ||]
#endif


{- | @I@, the identity matrix.

Neutral element of its monoid, so you can use 'mempty'.
-}
{-# INLINE identity #-}
identity :: Mat4
identity = fromMemory
  1 0 0 0
  0 1 0 0
  0 0 1 0
  0 0 0 1

-- TODO: simdify
{-# INLINE transpose #-}
transpose :: Mat4 -> Mat4
transpose =
  flip withMemory
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
    fromMemory
      e0 e4 e8 eC
      e1 e5 e9 eD
      e2 e6 eA eE
      e3 e7 eB eF

pointwise :: Mat4 -> Mat4 -> (Float -> Float -> Float) -> Mat4
pointwise a b f =
  withMemory a
    \ a0 a1 a2 a3
      a4 a5 a6 a7
      a8 a9 aA aB
      aC aD aE aF ->
  withMemory b
    \ b0 b1 b2 b3
      b4 b5 b6 b7
      b8 b9 bA bB
      bC bD bE bF ->
  fromMemory
    (f a0 b0) (f a1 b1) (f a2 b2) (f a3 b3)
    (f a4 b4) (f a5 b5) (f a6 b6) (f a7 b7)
    (f a8 b8) (f a9 b9) (f aA bA) (f aB bB)
    (f aC bC) (f aD bD) (f aE bE) (f aF bF)

{-# INLINE newMat4 #-}
newMat4 :: IO Mat4
newMat4 =
  IO \world ->
    let
      !(# world_, arr_ #) = newAlignedPinnedByteArray# 64# 16# world
      !(# _world', arr #) = unsafeFreezeByteArray# arr_ world_
    in
      (# world, Mat4 arr #)

{-# INLINE scalarMultiply #-}
scalarMultiply :: Float -> Mat4 -> Mat4
scalarMultiply x m =
  withMemory m
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
      fromMemory
      (e0 * x) (e1 * x) (e2 * x) (e3 * x)
      (e4 * x) (e5 * x) (e6 * x) (e7 * x)
      (e8 * x) (e9 * x) (eA * x) (eB * x)
      (eC * x) (eD * x) (eE * x) (eF * x)

instance Storable Mat4 where
  sizeOf _mat4 = 64

  alignment _mat4 = 16

  {-# INLINE poke #-}
  poke (Ptr addr) (Mat4 arr) = IO \world ->
    let
      world' = copyByteArrayToAddr# arr 0# addr 64# world
    in
      (# world', () #)

  {-# INLINE peek #-}
  peek (Ptr addr) = IO \world ->
    let
      !(# world0, arr #)  = newAlignedPinnedByteArray# 64# 16# world
      world1              = copyAddrToByteArray# addr arr 0# 64# world0
      !(# world', arr' #) = unsafeFreezeByteArray# arr world1
    in
      (# world', Mat4 arr' #)

instance Block Mat4 where
  type PackedSize Mat4 = 64
  alignment140 _  = 16
  sizeOf140       = sizeOfPacked
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

toListMemory :: Mat4 -> [Float]
toListMemory = flip withMemory
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
    [ e0, e1, e2, e3
    , e4, e5, e6, e7
    , e8, e9, eA, eB
    , eC, eD, eE, eF
    ]

toList2dMemory :: Mat4 -> [[Float]]
toList2dMemory = flip withMemory
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
    [ [e0, e1, e2, e3]
    , [e4, e5, e6, e7]
    , [e8, e9, eA, eB]
    , [eC, eD, eE, eF]
    ]

toListTrans :: Mat4 -> [Float]
toListTrans = flip withMemory
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
    [ e0, e4, e8, eC
    , e1, e5, e9, eD
    , e2, e6, eA, eE
    , e3, e7, eB, eF
    ]

toList2dTrans :: Mat4 -> [[Float]]
toList2dTrans = flip withMemory
    \ e0 e1 e2 e3
      e4 e5 e6 e7
      e8 e9 eA eB
      eC eD eE eF ->
    [ [e0, e4, e8, eC]
    , [e1, e5, e9, eD]
    , [e2, e6, eA, eE]
    , [e3, e7, eB, eF]
    ]

zipWith :: (Float -> Float -> c) -> Mat4 -> Mat4 -> [c]
zipWith f a b = List.zipWith f (toListMemory a) (toListMemory b)
