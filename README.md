# Geomancy

> Linear is nice, but slow. Those are naughty, but a bit faster.

* All data types are monomorphic, unpacked and specialized.
* `Mat4` and `Vec4` are `ByteArray#`.
* `Mat4`x`Mat4` and `Mat4`x`Vec4` is done with SIMD.
* Matrix construction states their element order.
* Transforms don't require transposition for GLSL `mat4`*`vec4`.

### The Numbers

Storing a list of 1000 transformations (e.g. rendering instance data):

```
benchmarking 4x4 poke/1000/geomancy
time                 11.76 μs   (11.66 μs .. 11.92 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 11.75 μs   (11.69 μs .. 11.86 μs)
std dev              283.4 ns   (199.0 ns .. 399.0 ns)
variance introduced by outliers: 26% (moderately inflated)
```

If you're willing to adjust your shaders, it's only 2.4 times slower.

```
benchmarking 4x4 poke/1000/linear
time                 28.29 μs   (28.21 μs .. 28.38 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 28.40 μs   (28.34 μs .. 28.50 μs)
std dev              267.4 ns   (145.5 ns .. 419.9 ns)
```

Keeping your shaders straight make the affair 6.1x slower.

```
benchmarking 4x4 poke/1000/linear/T
time                 73.70 μs   (73.06 μs .. 74.49 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 72.77 μs   (72.50 μs .. 73.22 μs)
std dev              1.129 μs   (793.5 ns .. 1.580 μs)
```

Folding down a `gloss`-style scene graph is where it is all started:

```
benchmarking 4x4 multiply/1000/geomancy
time                 20.79 μs   (20.77 μs .. 20.83 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 20.80 μs   (20.78 μs .. 20.83 μs)
std dev              76.71 ns   (60.01 ns .. 99.06 ns)

benchmarking 4x4 multiply/1000/linear
time                 173.9 μs   (173.6 μs .. 174.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 173.5 μs   (173.2 μs .. 174.4 μs)
std dev              1.733 μs   (727.8 ns .. 3.422 μs)
```

Add that time to the poking that'll follow.

Sure, it is in the lower microseconds range, but this budget can be used elsewhere.
