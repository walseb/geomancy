# Changelog for geomancy

## 0.2.4.0

+ Added `simple-affine-space` instances.
  * The classes are re-exported from `Geomancy`, bringing stuff like `dot` and `normalize`.
+ Added `Point2`, `Point3` wrappers.

## 0.2.3.0

- Vec4 moved to ByteArray#.
- Mat4 !* Vec4 is now SIMD too.

## 0.2.2.4

+ Add Transform trees.

## 0.2.2.3

+ Add IVec and UVec 32-bit integer vectors.

## 0.2.2.2

+ Fixed using Transform.apply with projection inverses.

## 0.2.2.1

+ Add Vec2.
+ Add pattern synonyms to `Geomancy` re-exports.
+ Add Fractional instances.
+ Add conversions from tuples and lower-dimension vecs.

## 0.2.2.0

+ Add pattern synonym alternatives to `withVecN`.
+ Add Mat4 converstion from `Linear.M44`.
- Hide `toList` and `toListTrans`.
+ Add `toList2d` and publish its element-order wrappers.

## 0.2.1.0

Transform rewrite

* Change perspective FoV to radians.
+ Add `infinitePerspective`.
+ Add `Transform.inverse`.
- Hide `mat4`, `withMat4`.

## 0.2.0.0

Mat4 rewrite

+ Add `rowMajor`, `withRowMajor`, `toListRowMajor`.
+ Add `colMajor`, `withColMajor`, `toListColMajor`.
+ Add `Mat4.inverse`.
* Expose `matrixProduct`.
+ Add `scalarMultiply`.
+ Add `Mat4.(!*)` to use with `Vec4`.
* Extract transformations to `Geomancy.Transform` and use column notation.
+ Add `Transform.(!.)` and `apply` to use with `Vec3`.
* Rename `scale` to `scale3`.
+ Add uniform `scale`.
+ Add `scaleXY` for flat meshes.
* Rename `mkTransformation` to `dirPos`.
+ Add `rotateQ` via `dirPos` with empty translation.
+ Add `Vec3.Packed` newtype without extra padding.

## 0.1.3.0

* Update tests
+ Add `zipWith`
* Rename `elementwise` to `pointwise`
* Rename `colMajor` to `toList`
* Rename `rowMajor` to `toListTrans`

## 0.1.2.1

+ Add Mat4 multiplication test via `linear`
+ Add `elementwise`, `colMajor`, `rowMajor`

## 0.1.2.0

* Move projections and views to Vulkan namespace.

## 0.1.1.2

* Fix bug in quaternion rotationBetween.

## 0.1.1.1

* Add lookAtUp and rotationBetween.
