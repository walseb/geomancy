module Geomancy.Transform.Tree
  ( Transformed
  , apply
  , node_
  , leaf_
  , collect_

  , translate_
  , translateV_

  , rotateX_
  , rotateY_
  , rotateZ_
  , rotateQ_

  , scale_
  , scaleX_
  , scaleY_
  , scaleZ_
  , scaleXY_
  , scale3_
  ) where

import Geomancy.Quaternion (Quaternion)
import Geomancy.Transform (Transform)
import Geomancy.Tree (Tree, apply, node_, leaf_, collect_)
import Geomancy.Vec3 (Vec3)

import qualified Geomancy.Transform as Transform

type Transformed a = Tree (Transform, Maybe a)

{-# INLINE translate_ #-}
translate_
  :: Float
  -> Float
  -> Float
  -> [Transformed a]
  -> Transformed a
translate_ x y z =
  node_ (Transform.translate x y z)

{-# INLINE translateV_ #-}
translateV_
  :: Vec3
  -> [Transformed a]
  -> Transformed a
translateV_ xyz =
  node_ (Transform.translateV xyz)

{-# INLINE rotateX_ #-}
rotateX_
  :: Float
  -> [Transformed a]
  -> Transformed a
rotateX_ rads =
  node_ (Transform.rotateX rads)

{-# INLINE rotateY_ #-}
rotateY_
  :: Float
  -> [Transformed a]
  -> Transformed a
rotateY_ rads =
  node_ (Transform.rotateY rads)

{-# INLINE rotateZ_ #-}
rotateZ_
  :: Float
  -> [Transformed a]
  -> Transformed a
rotateZ_ rads =
  node_ (Transform.rotateZ rads)

{-# INLINE rotateQ_ #-}
rotateQ_
  :: Quaternion
  -> [Transformed a]
  -> Transformed a
rotateQ_ rot =
  node_ (Transform.rotateQ rot)

{-# INLINE scale_ #-}
scale_
  :: Float
  -> [Transformed a]
  -> Transformed a
scale_ x =
  node_ (Transform.scale x)

{-# INLINE scaleX_ #-}
scaleX_
  :: Float
  -> [Transformed a]
  -> Transformed a
scaleX_ rads =
  node_ (Transform.scaleX rads)

{-# INLINE scaleY_ #-}
scaleY_
  :: Float
  -> [Transformed a]
  -> Transformed a
scaleY_ rads =
  node_ (Transform.scaleY rads)

{-# INLINE scaleZ_ #-}
scaleZ_
  :: Float
  -> [Transformed a]
  -> Transformed a
scaleZ_ rads =
  node_ (Transform.scaleZ rads)

{-# INLINE scaleXY_ #-}
scaleXY_
  :: Float
  -> Float
  -> [Transformed a]
  -> Transformed a
scaleXY_ x y =
  node_ (Transform.scaleXY x y)

{-# INLINE scale3_ #-}
scale3_
  :: Float
  -> Float
  -> Float
  -> [Transformed a]
  -> Transformed a
scale3_ x y z =
  node_ (Transform.scale3 x y z)
