{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Specialized and inlined @V2 Word32@.

module Geomancy.UVec2
  ( UVec2
  , uvec2
  , withUVec2
  , pattern WithUVec2
  , fromTuple
  ) where

import Control.DeepSeq (NFData(rnf))
import Data.Word (Word32)
import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))
import Foreign (Storable(..))

data UVec2 = UVec2
  {-# UNPACK #-} !Word32
  {-# UNPACK #-} !Word32
  deriving (Eq, Ord, Show)

{-# INLINE uvec2 #-}
uvec2 :: Word32 -> Word32 -> UVec2
uvec2 = UVec2

{-# INLINE withUVec2 #-}
withUVec2
  :: UVec2
  -> (Word32 -> Word32 -> r)
  -> r
withUVec2 (UVec2 a b) f = f a b

pattern WithUVec2 :: Word32 -> Word32 -> UVec2
pattern WithUVec2 a b <- ((`withUVec2` (,)) -> (a, b))
{-# COMPLETE WithUVec2 #-}

{-# INLINE fromTuple #-}
fromTuple :: (Word32, Word32) -> UVec2
fromTuple (x, y) = uvec2 x y

instance NFData UVec2 where
  rnf UVec2{} = ()

type instance Element UVec2 = Word32

instance MonoFunctor UVec2 where
  {-# INLINE omap #-}
  omap f v =
    withUVec2 v \x y ->
      uvec2 (f x) (f y)

instance MonoPointed UVec2 where
  {-# INLINE opoint #-}
  opoint x = uvec2 x x

-- XXX: That's one nasty instance...
instance Num UVec2 where
  {-# INLINE (+) #-}
  UVec2 l1 l2 + UVec2 r1 r2 =
    UVec2
      (l1 + r1)
      (l2 + r2)

  {-# INLINE (-) #-}
  UVec2 l1 l2 - UVec2 r1 r2 =
    UVec2
      (l1 - r1)
      (l2 - r2)

  {-# INLINE (*) #-}
  UVec2 l1 l2 * UVec2 r1 r2 =
    UVec2
      (l1 * r1)
      (l2 * r2)

  {-# INLINE abs #-}
  abs (UVec2 a b) =
    UVec2 (abs a) (abs b)

  {-# INLINE signum #-}
  signum v2 = withUVec2 v2 \a b ->
    uvec2 (signum a) (signum b)

  {-# INLINE fromInteger #-}
  fromInteger x = UVec2 x' x'
    where
      x' = fromInteger x

instance Storable UVec2 where
  {-# INLINE sizeOf #-}
  sizeOf _ = 8

  {-# INLINE alignment #-}
  alignment _ = 8

  {-# INLINE poke #-}
  poke ptr v4 =
    withUVec2 v4 \a b -> do
      pokeByteOff ptr 0 a
      pokeByteOff ptr 4 b

  {-# INLINE peek #-}
  peek ptr = uvec2
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
