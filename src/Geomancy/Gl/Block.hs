{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright :  (c) 2014-2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- OpenGL STD140 and STD430 support
--
-- Note STD430 can only be used for shader storage blocks, NOT
-- uniform blocks!
module Geomancy.Gl.Block
( Block(..)
, GBlock(..)
, Packed(..)
, STD140(..)
, STD430(..)
) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word32)
import Data.Int (Int32)
import Data.Proxy (Proxy(..))
import Foreign.Ptr (Ptr)
import Foreign.Ptr.Diff (Diff(..), peekDiffOff, pokeDiffOff)
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic, Rep, M1(..), K1(..), U1(..), C, D, S, (:*:)(..), from, to)
import Data.Data (Data, Typeable)

newtype Packed a = Packed { getPacked :: a }
  deriving (Data,Typeable,Generic,Functor,Foldable,Traversable,Eq,Ord,Show,Read)

instance Block a => Storable (Packed a) where
  alignment _ = 1
  sizeOf _ = sizeOfPacked (Proxy :: Proxy a)
  peekByteOff p o = Packed <$> readPacked p (Diff o)
  pokeByteOff p o = writePacked p (Diff o) . getPacked

newtype STD140 a = STD140 { getSTD140 :: a }
  deriving (Data,Typeable,Generic,Functor,Foldable,Traversable,Eq,Ord,Show,Read)

instance Block a => Storable (STD140 a) where
  alignment _ = alignment140 (Proxy :: Proxy a)
  sizeOf _ = sizeOf140 (Proxy :: Proxy a)
  peekByteOff p o = STD140 <$> read140 p (Diff o)
  pokeByteOff p o = write140 p (Diff o) . getSTD140

newtype STD430 a = STD430 { getSTD430 :: a }
  deriving (Data,Typeable,Generic,Functor,Foldable,Traversable,Eq,Ord,Show,Read)

instance Block a => Storable (STD430 a) where
  alignment _ = alignment430 (Proxy :: Proxy a)
  sizeOf _ = sizeOf430 (Proxy :: Proxy a)
  peekByteOff p o = STD430 <$> read430 p (Diff o)
  pokeByteOff p o = write430 p (Diff o) . getSTD430

-- | This describes how to load and store primitives
-- through a uniform/shader storage blocks according to
-- OpenGL STD140 and STD430.
--
-- There are lots of fiddly little constants around, beware.
class Block b where
  -- | As per 'Storable' 'alignment', but matching OpenGL STD140.
  alignment140 :: proxy b -> Int
  default alignment140 :: GBlock (Rep b) => proxy b -> Int
  alignment140 _ = galignment140 (Proxy :: Proxy (Rep b))

  -- | As per 'Storable' 'sizeOf', but matching OpenGL STD140.
  sizeOf140 :: proxy b -> Int
  default sizeOf140 :: GBlock (Rep b) => proxy b -> Int
  sizeOf140 _ = gsizeOf140 (Proxy :: Proxy (Rep b))

  -- | Structures get smashed up to a minimum of a vec4 alignment in 140 mode
  isStruct :: proxy b -> Bool
  isStruct _ = True

  read140 :: MonadIO m => Ptr a -> Diff a b -> m b
  default read140 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Diff a b -> m b
  read140 p (Diff o) = liftIO $ to <$> gread140 p o

  write140 :: MonadIO m => Ptr a -> Diff a b -> b -> m ()
  default write140 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Diff a b -> b -> m ()
  write140 p (Diff o) b = liftIO $ gwrite140 p o (from b)

  -- | As per 'Storable' 'alignment', but matching OpenGL STD430.
  alignment430 :: proxy b -> Int
  default alignment430 :: GBlock (Rep b) => proxy b -> Int
  alignment430 _ = galignment430 (Proxy :: Proxy (Rep b))

  -- | As per 'Storable' 'sizeOf', but matching OpenGL STD430.
  sizeOf430 :: proxy b -> Int
  default sizeOf430 :: GBlock (Rep b) => proxy b -> Int
  sizeOf430 _ = gsizeOf430 (Proxy :: Proxy (Rep b))

  read430 :: MonadIO m => Ptr a -> Diff a b -> m b
  default read430 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Diff a b -> m b
  read430 p (Diff o) = liftIO $ to <$> gread430 p o

  write430 :: MonadIO m => Ptr a -> Diff a b -> b -> m ()
  default write430 :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Diff a b -> b -> m ()
  write430 p (Diff o) b = liftIO $ gwrite430 p o (from b)

  -- | As per 'Storable' 'sizeOf', but without padding and no alignment
  sizeOfPacked :: proxy b -> Int
  default sizeOfPacked :: GBlock (Rep b) => proxy b -> Int
  sizeOfPacked _ = gsizeOfPacked (Proxy :: Proxy (Rep b))

  readPacked :: MonadIO m => Ptr a -> Diff a b -> m b
  default readPacked :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Diff a b -> m b
  readPacked p (Diff o) = liftIO $ to <$> greadPacked p o

  writePacked :: MonadIO m => Ptr a -> Diff a b -> b -> m ()
  default writePacked :: (MonadIO m, Generic b, GBlock (Rep b)) => Ptr a -> Diff a b -> b -> m ()
  writePacked p (Diff o) b = liftIO $ gwritePacked p o (from b)

-- | Automatically derive STD140 and STD430 alignment using GHC Generics
class GBlock f where
  galignment140    :: p f -> Int
  galignment430    :: p f -> Int
  gsizeOf140    :: p f -> Int
  gsizeOf430    :: p f -> Int
  gsizeOfPacked :: p f -> Int
  gread140    :: Ptr a -> Int -> IO (f b)
  gread430    :: Ptr a -> Int -> IO (f b)
  greadPacked :: Ptr a -> Int -> IO (f b)
  gwrite140    :: Ptr a -> Int -> f b -> IO ()
  gwrite430    :: Ptr a -> Int -> f b -> IO ()
  gwritePacked :: Ptr a -> Int -> f b -> IO ()

instance GBlock U1 where
  galignment140 _ = 1
  gsizeOf140    _ = 0
  galignment430 _ = 1
  gsizeOf430    _ = 0
  gsizeOfPacked    _ = 0
  gread140 _ _ = return U1
  gread430 _ _ = return U1
  greadPacked _ _ = return U1
  gwrite140 _ _ U1 = return ()
  gwrite430 _ _ U1 = return ()
  gwritePacked _ _ U1 = return ()

instance (GBlock f, GBlock g) => GBlock (f :*: g) where
  gsizeOfPacked _ = gsizeOfPacked (Proxy :: Proxy f) + gsizeOfPacked (Proxy :: Proxy g)
  galignment140 _ = galignment140 (Proxy :: Proxy f)
              `max` galignment140 (Proxy :: Proxy g)
  galignment430 _ = galignment430 (Proxy :: Proxy f)
              `max` galignment430 (Proxy :: Proxy g)
  gsizeOf140 _ = roundUp (gsizeOf140 (Proxy :: Proxy f)) (galignment140 (Proxy :: Proxy g)) + gsizeOf140 (Proxy :: Proxy g)
  gsizeOf430 _ = roundUp (gsizeOf430 (Proxy :: Proxy f)) (galignment430 (Proxy :: Proxy g)) + gsizeOf430 (Proxy :: Proxy g)
  gread140 p o = (:*:) <$> gread140 p o <*> gread140 p (o + roundUp (gsizeOf140 (Proxy :: Proxy f)) (galignment140 (Proxy :: Proxy g)))
  gread430 p o = (:*:) <$> gread430 p o <*> gread430 p (o + roundUp (gsizeOf430 (Proxy :: Proxy f)) (galignment430 (Proxy :: Proxy g)))
  greadPacked p o = (:*:) <$> greadPacked p o <*> greadPacked p (o + gsizeOfPacked (Proxy :: Proxy f))
  gwrite140 p o (a :*: b) = do
    gwrite140 p o a
    gwrite140 p (o + roundUp (gsizeOf140 (Proxy :: Proxy f)) (galignment140 (Proxy :: Proxy g))) b
  gwrite430 p o (a :*: b) = do
    gwrite430 p o a
    gwrite430 p (o + roundUp (gsizeOf430 (Proxy :: Proxy f)) (galignment430 (Proxy :: Proxy g))) b
  gwritePacked p o (a :*: b) = do
    gwritePacked p o a
    gwritePacked p (o + gsizeOfPacked (Proxy :: Proxy f)) b

instance GBlock f => GBlock (M1 S c f) where
  galignment140    _ = galignment140 (Proxy :: Proxy f)
  galignment430    _ = galignment430 (Proxy :: Proxy f)
  gsizeOf140    _ = gsizeOf140 (Proxy :: Proxy f)
  gsizeOf430    _ = gsizeOf430 (Proxy :: Proxy f)
  gsizeOfPacked _ = gsizeOfPacked (Proxy :: Proxy f)
  gread140 p o = M1 <$> gread140 p o
  gread430 p o = M1 <$> gread430 p o
  greadPacked p o = M1 <$> greadPacked p o
  gwrite140 p o (M1 a) = gwrite140 p o a
  gwrite430 p o (M1 a) = gwrite430 p o a
  gwritePacked p o (M1 a) = gwritePacked p o a

instance GBlock f => GBlock (M1 C c f) where
  galignment140    _ = lcm 16 $ galignment140 (Proxy :: Proxy f) -- std140 rule 9
  galignment430    _ = galignment430 (Proxy :: Proxy f) -- std140 rule 9, relaxed by std430
  gsizeOf140    _ = roundUp (gsizeOf140 (Proxy :: Proxy f)) (galignment140 (Proxy :: Proxy f)) -- std140 rule 9
  gsizeOf430    _ = roundUp (gsizeOf430 (Proxy :: Proxy f)) (galignment430 (Proxy :: Proxy f)) -- std140 rule 9, relaxed by std430
  gsizeOfPacked _ = gsizeOfPacked (Proxy :: Proxy f)
  gread140 p o = M1 <$> gread140 p o
  gread430 p o = M1 <$> gread430 p o
  greadPacked p o = M1 <$> greadPacked p o
  gwrite140 p o (M1 a) = gwrite140 p o a
  gwrite430 p o (M1 a) = gwrite430 p o a
  gwritePacked p o (M1 a) = gwritePacked p o a

instance GBlock f => GBlock (M1 D c f) where
  galignment140 _ = galignment140 (Proxy :: Proxy f)
  galignment430 _ = galignment430 (Proxy :: Proxy f)
  gsizeOf140    _ = gsizeOf140 (Proxy :: Proxy f)
  gsizeOf430    _ = gsizeOf430 (Proxy :: Proxy f)
  gsizeOfPacked    _ = gsizeOfPacked (Proxy :: Proxy f)
  gread140 p o = M1 <$> gread140 p o
  gread430 p o = M1 <$> gread430 p o
  greadPacked p o = M1 <$> greadPacked p o
  gwrite140 p o (M1 a) = gwrite140 p o a
  gwrite430 p o (M1 a) = gwrite430 p o a
  gwritePacked p o (M1 a) = gwritePacked p o a

instance Block c => GBlock (K1 i c) where
  galignment140 _ = alignment140 (Proxy :: Proxy c)
  galignment430 _ = alignment430 (Proxy :: Proxy c)
  gsizeOf140    _ = sizeOf140 (Proxy :: Proxy c)
  gsizeOf430    _ = sizeOf430 (Proxy :: Proxy c)
  gsizeOfPacked    _ = sizeOfPacked (Proxy :: Proxy c)
  gread140 p o = K1 <$> read140 p (Diff o)
  gread430 p o = K1 <$> read430 p (Diff o)
  greadPacked p o = K1 <$> readPacked p (Diff o)
  gwrite140 p o (K1 a) = write140 p (Diff o) a
  gwrite430 p o (K1 a) = write430 p (Diff o) a
  gwritePacked p o (K1 a) = writePacked p (Diff o) a

toBool :: Int32 -> Bool
toBool 0 = False
toBool _ = True

fromBool :: Bool -> Int32
fromBool False = 0
fromBool True = 1

instance Block Bool where
  sizeOfPacked _ = 4
  alignment140 _ = 4
  sizeOf140      = sizeOfPacked
  alignment430   = alignment140
  sizeOf430      = sizeOf140
  isStruct _     = False
  read140 p (Diff d) = fmap toBool $ peekDiffOff p (Diff d)
  write140 p (Diff d) = pokeDiffOff p (Diff d) . fromBool
  read430     = read140
  write430    = write140
  readPacked  = read140
  writePacked = write140
  {-# INLINE sizeOfPacked #-}
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

instance Block Int32 where
  sizeOfPacked _ = 4
  alignment140 _ = 4
  sizeOf140      = sizeOfPacked
  alignment430   = alignment140
  sizeOf430      = sizeOf140
  isStruct _     = False
  read140     = peekDiffOff
  write140    = pokeDiffOff
  read430     = read140
  write430    = write140
  readPacked  = read140
  writePacked = write140
  {-# INLINE sizeOfPacked #-}
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

instance Block Word32 where
  sizeOfPacked _ = 4
  alignment140 _ = 4
  sizeOf140      = sizeOfPacked
  alignment430   = alignment140
  sizeOf430      = sizeOf140
  isStruct _     = False
  read140     = peekDiffOff
  write140    = pokeDiffOff
  read430     = read140
  write430    = write140
  readPacked  = read140
  writePacked = write140
  {-# INLINE sizeOfPacked #-}
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

instance Block Float where
  sizeOfPacked _ = 4
  alignment140 _ = 4
  sizeOf140      = sizeOfPacked
  alignment430   = alignment140
  sizeOf430      = sizeOf140
  isStruct _     = False
  read140     = peekDiffOff
  write140    = pokeDiffOff
  read430     = read140
  write430    = write140
  readPacked  = read140
  writePacked = write140
  {-# INLINE sizeOfPacked #-}
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

instance Block Double where
  sizeOfPacked _ = 8
  alignment140 _ = 8
  sizeOf140      = sizeOfPacked
  alignment430   = alignment140
  sizeOf430      = sizeOf140
  isStruct _     = False
  read140     = peekDiffOff
  write140    = pokeDiffOff
  read430     = read140
  write430    = write140
  readPacked  = read140
  writePacked = write140
  {-# INLINE sizeOfPacked #-}
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

-- -- | Can be used for fixed-sized arrays
-- instance (Dim n, Block a) => Block (V n a) where
--   isStruct _ = isStruct (Proxy :: Proxy a)
--   alignment140 _
--     | isStruct (Proxy :: Proxy a) = lcm 16 n -- std140 rule 9
--     | otherwise = n
--     where n = alignment140 (Proxy :: Proxy a)
--   alignment430 _ = alignment430 (Proxy :: Proxy a)
--   sizeOf140 _ = roundUp (sizeOf140 (Proxy :: Proxy a)) (alignment140 (Proxy :: Proxy a)) * reflectDim (Proxy :: Proxy n)
--   sizeOf430 _ = roundUp (sizeOf430 (Proxy :: Proxy a)) (alignment430 (Proxy :: Proxy a)) * reflectDim (Proxy :: Proxy n)
--   read140 p (Diff o) = liftIO $ sequence $ tabulate \i -> read140 p $ Diff (o + i*d) where
--     d = roundUp (sizeOf140 (Proxy :: Proxy a)) (alignment140 (Proxy :: Proxy a))
--   write140 p (Diff o) v = liftIO $ iforM_ v \i -> write140 p (Diff (o + i*d)) where
--     d = roundUp (sizeOf140 (Proxy :: Proxy a)) (alignment140 (Proxy :: Proxy a))
--   read430 p (Diff o) = liftIO $ sequence $ tabulate \i -> read430 p $ Diff (o + i*d) where
--     d = roundUp (sizeOf430 (Proxy :: Proxy a)) (alignment430 (Proxy :: Proxy a))
--   write430 p (Diff o) v = liftIO $ iforM_ v \i -> write430 p (Diff (o + i*d)) where
--     d = roundUp (sizeOf430 (Proxy :: Proxy a)) (alignment430 (Proxy :: Proxy a))

-- | @roundUp k n@ rounds up k up to an integral multiple of n
roundUp :: Int -> Int -> Int
roundUp k n = k + mod (n - k) n

instance (Block a, Block b) => Block (a, b)
instance (Block a, Block b, Block c) => Block (a, b, c)
