{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}

module Geomancy.Elementwise
  ( Elementwise(..)
  , Element
  ) where

import Data.MonoTraversable (Element, MonoFunctor(..), MonoPointed(..))

class Elementwise a where
  epoint :: Element a -> a

  default epoint :: MonoPointed a => Element a -> a
  epoint = opoint

  emap
    :: (Element a -> Element a)
    -> a -> a
  default emap
    :: MonoFunctor a
    => (Element a -> Element a)
    -> a -> a
  emap = omap

  emap2
    :: (Element a -> Element a -> Element a)
    -> a -> a -> a

  emap3
    :: (Element a -> Element a -> Element a -> Element a)
    -> a -> a -> a -> a

  emap4
    :: (Element a -> Element a -> Element a -> Element a -> Element a)
    -> a -> a -> a -> a -> a

  emap5
    :: (Element a -> Element a -> Element a -> Element a -> Element a -> Element a)
    -> a -> a -> a -> a -> a -> a
