# Changelog for geomancy

## 0.3.0.0

> `[0]` releases without row/col-major, handedness, and counter/clockwise confusion.

This release brings sanity and consistency around the transform stack.
- `Transform`s have proper *col-major* definitions.
- Operation ordering follows the GLSL convention.
  * The composition order is `(p <> v <> m <> ...)` (from global to local).
    Reverse your transforms to adjust.
  * The application order is `m !* v` (with the operators becoming infixr 5, just under the `<>`).
    It will do `Mᵀ * v` inside for SIMD reasons and to match what GLSL does for its `M * v` ops.
- `Geomancy.Vulkan.Projection` is right-handed, BUT produces *reverse-depth* range ([1; 0], In 3D infinite-Z converges to 0).
  * Replace your depth tests with `OP_GREATER` and clear to `0.0` - get better precision for your migration troubles.
- `Geomancy.Vulkan.View` is *right-handed*, with +Z being forward.
  * The intended up vector is still `vec3 0 (-1) 0` -- +Y down.
    Silly as it sounds, this matches the XY plane of the window with XY plane in front of a "first person" camera.
- Axis rotations (using `rotateQ`) will appear clockwise when looking along the axis.
- Angle rotations follow Tait-Bryan angles (heading/elevation/bank or yaw/pitch/roll) in the y-x-z frame.
  * `rotateZ (time * rate)` will follow the clock hands in 2D scenes and roll in 3D.
  * `rotateX` will follow the sun from sunrise to sunset, pitching UP / increasing elevation.
  * `rotateY` will turn you right, increasing yaw / heading eastwards.
- You're of course free to define your own transforms, just copy the modules and tune to your liking.
  Just make sure that you use matching row/column constructors and the math layer will do the rest, fast.
- Added `webcolor-labels` instances for UVec3/Vec3/Vec4.

## 0.2.6.0

* `Geomancy.Gl.Block` extract to `gl-block` package as `Graphics.Gl.Block`.
* Added `convert` function to vector modules to facilitate type-changing operations like rounding.
* Added `Ix` instances for integral vectors.
* Added `dot` for integral vectors.

## 0.2.5.0

* Added Geomancy.Gl.Block to derive packed/std140/std430 layouts generically.
  Originally a part of the unpublished `glow` package in the `codex` project by Edward Kmett.
  Add `ptrdiff-0` to your stack resolvers.

## 0.2.4.2

* Support ARM/aarch64 SIMD.

## 0.2.4.1

* Support simple-affine-space-0.2

## 0.2.4.0

A bunch of experimental code to see what sticks.

+ Added `simple-affine-space` instances.
  * The classes are re-exported from `Geomancy`, bringing stuff like `dot` and `normalize`.
+ Added `mono-traversable` instances and `Geomancy.Elementwise` wrapper.
+ Added `Point` wrapper and `PointN` aliases.
+ Added `Geomancy.Interpolate` with generic linear, quadratic and cubic functions.
  - ⚠️ `VecN.lerp` has wrong (flipped) order of vector arguments. This is fixed in `linear`.
+ Added `Geomancy.Swizzle` overloaded labels.
+ Added `Geomancy.Gl.Funs` with kinda-GLSL math functions, appropriately overloaded.

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
