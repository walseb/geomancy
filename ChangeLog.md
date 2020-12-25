# Changelog for geomancy

## Unreleased changes

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
