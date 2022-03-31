{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Geomancy.Swizzle
  ( swizzle
  , (.@)
  ) where

import GHC.OverloadedLabels

import Geomancy.Vec2 (Vec2, vec2, withVec2)
import Geomancy.Vec3 (Vec3, vec3, withVec3)
import Geomancy.Vec4 (Vec4, vec4, withVec4)

newtype Swizzle to fro = Swizzle { swizzle :: fro -> to }

{-# INLINE (.@) #-}
(.@) :: forall to fro . fro -> Swizzle to fro -> to
v .@ Swizzle f = f v

-- * From Vec2

-- ** To Float

instance IsLabel "x" (Swizzle Float Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x _ -> x

instance IsLabel "y" (Swizzle Float Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \_ y -> y

-- ** To Vec2

instance IsLabel "xx" (Swizzle Vec2 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x _ -> vec2 x x

instance IsLabel "yx" (Swizzle Vec2 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec2 y x

instance IsLabel "yy" (Swizzle Vec2 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \_ y -> vec2 y y

-- ** To Vec3

instance IsLabel "xxx" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x _ -> vec3 x x x

instance IsLabel "xxy" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec3 x x y

instance IsLabel "xyx" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec3 x y x

instance IsLabel "xyy" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec3 x y y

instance IsLabel "yxx" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec3 y x x

instance IsLabel "yxy" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec3 y x y

instance IsLabel "yyx" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec3 y y x

instance IsLabel "yyy" (Swizzle Vec3 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \_ y -> vec3 y y y

-- ** To Vec4

instance IsLabel "xxxx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x _ -> vec4 x x x x

instance IsLabel "xxxy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 x x x y

instance IsLabel "xxyx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 x x y x

instance IsLabel "xxyy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 x x y y

instance IsLabel "xyxx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 x y x x

instance IsLabel "xyxy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 x y x y

instance IsLabel "xyyx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 x y y x

instance IsLabel "xyyy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 x y y y

instance IsLabel "yxxx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 y x x x

instance IsLabel "yxxy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 y x x y

instance IsLabel "yxyx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 y x y x

instance IsLabel "yxyy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 y x y y

instance IsLabel "yyxx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 y y x x

instance IsLabel "yyxy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 y y x y

instance IsLabel "yyyx" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \x y -> vec4 y y y x

instance IsLabel "yyyy" (Swizzle Vec4 Vec2) where
  fromLabel = Swizzle \v -> withVec2 v \_ y -> vec4 y y y y

-- * From Vec3

-- * To Float

instance IsLabel "x" (Swizzle Float Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ _ -> x

instance IsLabel "y" (Swizzle Float Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y _ -> y

instance IsLabel "z" (Swizzle Float Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ _ z -> z

-- * To Vec2

instance IsLabel "xx" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ _ -> vec2 x x

instance IsLabel "xy" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec2 x y

instance IsLabel "xz" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec2 x z

instance IsLabel "yx" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec2 y x

instance IsLabel "yy" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y _ -> vec2 y y

instance IsLabel "yz" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec2 y z

instance IsLabel "zx" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec2 z x

instance IsLabel "zy" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec2 z y

instance IsLabel "zz" (Swizzle Vec2 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ _ z -> vec2 z z

-- * To Vec3

instance IsLabel "xxx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ _ -> vec3 x x x

instance IsLabel "xxy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec3 x x y

instance IsLabel "xxz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec3 x x z

instance IsLabel "xyx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec3 x y x

instance IsLabel "xyy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec3 x y y

instance IsLabel "xzx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec3 x z x

instance IsLabel "xzy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec3 x z y

instance IsLabel "xzz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec3 x z z

instance IsLabel "yxx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec3 y x x

instance IsLabel "yxy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec3 y x y

instance IsLabel "yxz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec3 y x z

instance IsLabel "yyx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec3 y y x

instance IsLabel "yyy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y _ -> vec3 y y y

instance IsLabel "yyz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec3 y y z

instance IsLabel "yzx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec3 y z x

instance IsLabel "yzy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec3 y z y

instance IsLabel "yzz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec3 y z z

instance IsLabel "zxx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec3 z x x

instance IsLabel "zxy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec3 z x y

instance IsLabel "zxz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec3 z x z

instance IsLabel "zyx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec3 z y x

instance IsLabel "zyy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec3 z y y

instance IsLabel "zyz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec3 z y z

instance IsLabel "zzx" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec3 z z x

instance IsLabel "zzy" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec3 z z y

instance IsLabel "zzz" (Swizzle Vec3 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ _ z -> vec3 z z z

-- * To Vec4

instance IsLabel "xxxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ _ -> vec4 x x x x

instance IsLabel "xxxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 x x x y

instance IsLabel "xxxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 x x x z

instance IsLabel "xxyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 x x y x

instance IsLabel "xxyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 x x y y

instance IsLabel "xxyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x x y z

instance IsLabel "xxzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 x x z x

instance IsLabel "xxzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x x z y

instance IsLabel "xxzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 x x z z

instance IsLabel "xyxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 x y x x

instance IsLabel "xyxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 x y x y

instance IsLabel "xyxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x y x z

instance IsLabel "xyyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 x y y x

instance IsLabel "xyyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 x y y y

instance IsLabel "xyyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x y y z

instance IsLabel "xyzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x y z x

instance IsLabel "xyzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x y z y

instance IsLabel "xyzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x y z z

instance IsLabel "xzxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 x z x x

instance IsLabel "xzxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x z x y

instance IsLabel "xzxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 x z x z

instance IsLabel "xzyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x z y x

instance IsLabel "xzyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x z y y

instance IsLabel "xzyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x z y z

instance IsLabel "xzzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 x z z x

instance IsLabel "xzzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 x z z y

instance IsLabel "xzzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 x z z z

instance IsLabel "yxxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 y x x x

instance IsLabel "yxxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 y x x y

instance IsLabel "yxxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y x x z

instance IsLabel "yxyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 y x y x

instance IsLabel "yxyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 y x y y

instance IsLabel "yxyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y x y z

instance IsLabel "yxzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y x z x

instance IsLabel "yxzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y x z y

instance IsLabel "yxzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y x z z

instance IsLabel "yyxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 y y x x

instance IsLabel "yyxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 y y x y

instance IsLabel "yyxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y y x z

instance IsLabel "yyyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y _ -> vec4 y y y x

instance IsLabel "yyyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y _ -> vec4 y y y y

instance IsLabel "yyyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 y y y z

instance IsLabel "yyzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y y z x

instance IsLabel "yyzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 y y z y

instance IsLabel "yyzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 y y z z

instance IsLabel "yzxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y z x x

instance IsLabel "yzxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y z x y

instance IsLabel "yzxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y z x z

instance IsLabel "yzyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y z y x

instance IsLabel "yzyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 y z y y

instance IsLabel "yzyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 y z y z

instance IsLabel "yzzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 y z z x

instance IsLabel "yzzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 y z z y

instance IsLabel "yzzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 y z z z

instance IsLabel "zxxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 z x x x

instance IsLabel "zxxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z x x y

instance IsLabel "zxxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 z x x z

instance IsLabel "zxyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z x y x

instance IsLabel "zxyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z x y y

instance IsLabel "zxyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z x y z

instance IsLabel "zxzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 z x z x

instance IsLabel "zxzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z x z y

instance IsLabel "zxzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 z x z z

instance IsLabel "zyxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z y x x

instance IsLabel "zyxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z y x y

instance IsLabel "zyxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z y x z

instance IsLabel "zyyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z y y x

instance IsLabel "zyyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 z y y y

instance IsLabel "zyyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 z y y z

instance IsLabel "zyzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z y z x

instance IsLabel "zyzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 z y z y

instance IsLabel "zyzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 z y z z

instance IsLabel "zzxx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 z z x x

instance IsLabel "zzxy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z z x y

instance IsLabel "zzxz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 z z x z

instance IsLabel "zzyx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x y z -> vec4 z z y x

instance IsLabel "zzyy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 z z y y

instance IsLabel "zzyz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 z z y z

instance IsLabel "zzzx" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \x _ z -> vec4 z z z x

instance IsLabel "zzzy" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ y z -> vec4 z z z y

instance IsLabel "zzzz" (Swizzle Vec4 Vec3) where
  fromLabel = Swizzle \v -> withVec3 v \_ _ z -> vec4 z z z z

-- * From Vec4

-- ** To Float

instance IsLabel "x" (Swizzle Float Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ _ -> x

instance IsLabel "y" (Swizzle Float Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ _ -> y

instance IsLabel "z" (Swizzle Float Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z _ -> z

instance IsLabel "w" (Swizzle Float Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ _ w -> w

-- ** To Vec2

instance IsLabel "xx" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ _ -> vec2 x x

instance IsLabel "xy" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec2 x y

instance IsLabel "xz" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec2 x z

instance IsLabel "xw" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec2 x w

instance IsLabel "yx" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec2 y x

instance IsLabel "yy" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ _ -> vec2 y y

instance IsLabel "yz" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec2 y z

instance IsLabel "yw" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec2 y w

instance IsLabel "zx" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec2 z x

instance IsLabel "zy" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec2 z y

instance IsLabel "zz" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z _ -> vec2 z z

instance IsLabel "zw" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec2 z w

instance IsLabel "wx" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec2 w x

instance IsLabel "wy" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec2 w y

instance IsLabel "wz" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec2 w z

instance IsLabel "ww" (Swizzle Vec2 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ _ w -> vec2 w w

-- ** To Vec3

instance IsLabel "xxx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ _ -> vec3 x x x

instance IsLabel "xxy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec3 x x y

instance IsLabel "xxz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec3 x x z

instance IsLabel "xxw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec3 x x w

instance IsLabel "xyx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec3 x y x

instance IsLabel "xyy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec3 x y y

instance IsLabel "xyz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec3 x y z

instance IsLabel "xyw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec3 x y w

instance IsLabel "xzx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec3 x z x

instance IsLabel "xzy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec3 x z y

instance IsLabel "xzz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec3 x z z

instance IsLabel "xzw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec3 x z w

instance IsLabel "xwx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec3 x w x

instance IsLabel "xwy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec3 x w y

instance IsLabel "xwz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec3 x w z

instance IsLabel "xww" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec3 x w w

instance IsLabel "yxx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec3 y x x

instance IsLabel "yxy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec3 y x y

instance IsLabel "yxz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec3 y x z

instance IsLabel "yxw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec3 y x w

instance IsLabel "yyx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec3 y y x

instance IsLabel "yyy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ _ -> vec3 y y y

instance IsLabel "yyz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec3 y y z

instance IsLabel "yyw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec3 y y w

instance IsLabel "yzx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec3 y z x

instance IsLabel "yzy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec3 y z y

instance IsLabel "yzz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec3 y z z

instance IsLabel "yzw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec3 y z w

instance IsLabel "ywx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec3 y w x

instance IsLabel "ywy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec3 y w y

instance IsLabel "ywz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec3 y w z

instance IsLabel "yww" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec3 y w w

instance IsLabel "zxx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec3 z x x

instance IsLabel "zxy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec3 z x y

instance IsLabel "zxz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec3 z x z

instance IsLabel "zxw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec3 z x w

instance IsLabel "zyx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec3 z y x

instance IsLabel "zyy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec3 z y y

instance IsLabel "zyz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec3 z y z

instance IsLabel "zyw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec3 z y w

instance IsLabel "zzx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec3 z z x

instance IsLabel "zzy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec3 z z y

instance IsLabel "zzz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z _ -> vec3 z z z

instance IsLabel "zzw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec3 z z w

instance IsLabel "zwx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec3 z w x

instance IsLabel "zwy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec3 z w y

instance IsLabel "zwz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec3 z w z

instance IsLabel "zww" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec3 z w w

instance IsLabel "wxx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec3 w x x

instance IsLabel "wxy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec3 w x y

instance IsLabel "wxz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec3 w x z

instance IsLabel "wxw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec3 w x w

instance IsLabel "wyx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec3 w y x

instance IsLabel "wyy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec3 w y y

instance IsLabel "wyz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec3 w y z

instance IsLabel "wyw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec3 w y w

instance IsLabel "wzx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec3 w z x

instance IsLabel "wzy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec3 w z y

instance IsLabel "wzz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec3 w z z

instance IsLabel "wzw" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec3 w z w

instance IsLabel "wwx" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec3 w w x

instance IsLabel "wwy" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec3 w w y

instance IsLabel "wwz" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec3 w w z

instance IsLabel "www" (Swizzle Vec3 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ _ w -> vec3 w w w

-- ** To Vec4

instance IsLabel "xxxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ _ -> vec4 x x x x

instance IsLabel "xxxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 x x x y

instance IsLabel "xxxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 x x x z

instance IsLabel "xxxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 x x x w

instance IsLabel "xxyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 x x y x

instance IsLabel "xxyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 x x y y

instance IsLabel "xxyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x x y z

instance IsLabel "xxyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x x y w

instance IsLabel "xxzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 x x z x

instance IsLabel "xxzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x x z y

instance IsLabel "xxzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 x x z z

instance IsLabel "xxzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x x z w

instance IsLabel "xxwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 x x w x

instance IsLabel "xxwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x x w y

instance IsLabel "xxwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x x w z

instance IsLabel "xxww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 x x w w

instance IsLabel "xyxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 x y x x

instance IsLabel "xyxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 x y x y

instance IsLabel "xyxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x y x z

instance IsLabel "xyxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x y x w

instance IsLabel "xyyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 x y y x

instance IsLabel "xyyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 x y y y

instance IsLabel "xyyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x y y z

instance IsLabel "xyyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x y y w

instance IsLabel "xyzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x y z x

instance IsLabel "xyzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x y z y

instance IsLabel "xyzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x y z z

instance IsLabel "xywx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x y w x

instance IsLabel "xywy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x y w y

instance IsLabel "xywz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 x y w z

instance IsLabel "xyww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x y w w

instance IsLabel "xzxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 x z x x

instance IsLabel "xzxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x z x y

instance IsLabel "xzxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 x z x z

instance IsLabel "xzxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x z x w

instance IsLabel "xzyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x z y x

instance IsLabel "xzyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x z y y

instance IsLabel "xzyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x z y z

instance IsLabel "xzyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 x z y w

instance IsLabel "xzzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 x z z x

instance IsLabel "xzzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 x z z y

instance IsLabel "xzzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 x z z z

instance IsLabel "xzzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x z z w

instance IsLabel "xzwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x z w x

instance IsLabel "xzwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 x z w y

instance IsLabel "xzwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x z w z

instance IsLabel "xzww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x z w w

instance IsLabel "xwxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 x w x x

instance IsLabel "xwxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x w x y

instance IsLabel "xwxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x w x z

instance IsLabel "xwxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 x w x w

instance IsLabel "xwyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x w y x

instance IsLabel "xwyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x w y y

instance IsLabel "xwyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 x w y z

instance IsLabel "xwyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x w y w

instance IsLabel "xwzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x w z x

instance IsLabel "xwzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 x w z y

instance IsLabel "xwzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x w z z

instance IsLabel "xwzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x w z w

instance IsLabel "xwwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 x w w x

instance IsLabel "xwwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 x w w y

instance IsLabel "xwwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 x w w z

instance IsLabel "xwww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 x w w w

instance IsLabel "yxxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 y x x x

instance IsLabel "yxxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 y x x y

instance IsLabel "yxxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y x x z

instance IsLabel "yxxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y x x w

instance IsLabel "yxyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 y x y x

instance IsLabel "yxyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 y x y y

instance IsLabel "yxyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y x y z

instance IsLabel "yxyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y x y w

instance IsLabel "yxzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y x z x

instance IsLabel "yxzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y x z y

instance IsLabel "yxzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y x z z

instance IsLabel "yxzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 y x z w

instance IsLabel "yxwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y x w x

instance IsLabel "yxwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y x w y

instance IsLabel "yxwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 y x w z

instance IsLabel "yxww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y x w w

instance IsLabel "yyxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 y y x x

instance IsLabel "yyxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 y y x y

instance IsLabel "yyxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y y x z

instance IsLabel "yyxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y y x w

instance IsLabel "yyyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ _ -> vec4 y y y x

instance IsLabel "yyyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ _ -> vec4 y y y y

instance IsLabel "yyyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 y y y z

instance IsLabel "yyyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 y y y w

instance IsLabel "yyzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y y z x

instance IsLabel "yyzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 y y z y

instance IsLabel "yyzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 y y z z

instance IsLabel "yyzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y y z w

instance IsLabel "yywx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y y w x

instance IsLabel "yywy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 y y w y

instance IsLabel "yywz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y y w z

instance IsLabel "yyww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 y y w w

instance IsLabel "yzxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y z x x

instance IsLabel "yzxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y z x y

instance IsLabel "yzxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y z x z

instance IsLabel "yzxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 y z x w

instance IsLabel "yzyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y z y x

instance IsLabel "yzyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 y z y y

instance IsLabel "yzyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 y z y z

instance IsLabel "yzyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y z y w

instance IsLabel "yzzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 y z z x

instance IsLabel "yzzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 y z z y

instance IsLabel "yzzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 y z z z

instance IsLabel "yzzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y z z w

instance IsLabel "yzwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 y z w x

instance IsLabel "yzwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y z w y

instance IsLabel "yzwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y z w z

instance IsLabel "yzww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y z w w

instance IsLabel "ywxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y w x x

instance IsLabel "ywxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y w x y

instance IsLabel "ywxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 y w x z

instance IsLabel "ywxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y w x w

instance IsLabel "ywyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y w y x

instance IsLabel "ywyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 y w y y

instance IsLabel "ywyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y w y z

instance IsLabel "ywyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 y w y w

instance IsLabel "ywzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 y w z x

instance IsLabel "ywzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y w z y

instance IsLabel "ywzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y w z z

instance IsLabel "ywzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y w z w

instance IsLabel "ywwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 y w w x

instance IsLabel "ywwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 y w w y

instance IsLabel "ywwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 y w w z

instance IsLabel "ywww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 y w w w

instance IsLabel "zxxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 z x x x

instance IsLabel "zxxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z x x y

instance IsLabel "zxxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 z x x z

instance IsLabel "zxxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z x x w

instance IsLabel "zxyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z x y x

instance IsLabel "zxyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z x y y

instance IsLabel "zxyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z x y z

instance IsLabel "zxyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 z x y w

instance IsLabel "zxzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 z x z x

instance IsLabel "zxzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z x z y

instance IsLabel "zxzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 z x z z

instance IsLabel "zxzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z x z w

instance IsLabel "zxwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z x w x

instance IsLabel "zxwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 z x w y

instance IsLabel "zxwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z x w z

instance IsLabel "zxww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z x w w

instance IsLabel "zyxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z y x x

instance IsLabel "zyxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z y x y

instance IsLabel "zyxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z y x z

instance IsLabel "zyxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 z y x w

instance IsLabel "zyyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z y y x

instance IsLabel "zyyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 z y y y

instance IsLabel "zyyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 z y y z

instance IsLabel "zyyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z y y w

instance IsLabel "zyzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z y z x

instance IsLabel "zyzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 z y z y

instance IsLabel "zyzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 z y z z

instance IsLabel "zyzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z y z w

instance IsLabel "zywx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 z y w x

instance IsLabel "zywy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z y w y

instance IsLabel "zywz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z y w z

instance IsLabel "zyww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z y w w

instance IsLabel "zzxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 z z x x

instance IsLabel "zzxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z z x y

instance IsLabel "zzxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 z z x z

instance IsLabel "zzxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z z x w

instance IsLabel "zzyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z _ -> vec4 z z y x

instance IsLabel "zzyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 z z y y

instance IsLabel "zzyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 z z y z

instance IsLabel "zzyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z z y w

instance IsLabel "zzzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z _ -> vec4 z z z x

instance IsLabel "zzzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z _ -> vec4 z z z y

instance IsLabel "zzzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z _ -> vec4 z z z z

instance IsLabel "zzzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 z z z w

instance IsLabel "zzwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z z w x

instance IsLabel "zzwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z z w y

instance IsLabel "zzwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 z z w z

instance IsLabel "zzww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 z z w w

instance IsLabel "zwxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z w x x

instance IsLabel "zwxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 z w x y

instance IsLabel "zwxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z w x z

instance IsLabel "zwxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z w x w

instance IsLabel "zwyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 z w y x

instance IsLabel "zwyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z w y y

instance IsLabel "zwyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z w y z

instance IsLabel "zwyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z w y w

instance IsLabel "zwzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z w z x

instance IsLabel "zwzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z w z y

instance IsLabel "zwzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 z w z z

instance IsLabel "zwzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 z w z w

instance IsLabel "zwwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 z w w x

instance IsLabel "zwwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 z w w y

instance IsLabel "zwwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 z w w z

instance IsLabel "zwww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 z w w w

instance IsLabel "wxxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 w x x x

instance IsLabel "wxxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w x x y

instance IsLabel "wxxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w x x z

instance IsLabel "wxxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 w x x w

instance IsLabel "wxyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w x y x

instance IsLabel "wxyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w x y y

instance IsLabel "wxyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 w x y z

instance IsLabel "wxyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w x y w

instance IsLabel "wxzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w x z x

instance IsLabel "wxzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 w x z y

instance IsLabel "wxzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w x z z

instance IsLabel "wxzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w x z w

instance IsLabel "wxwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 w x w x

instance IsLabel "wxwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w x w y

instance IsLabel "wxwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w x w z

instance IsLabel "wxww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 w x w w

instance IsLabel "wyxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w y x x

instance IsLabel "wyxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w y x y

instance IsLabel "wyxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 w y x z

instance IsLabel "wyxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w y x w

instance IsLabel "wyyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w y y x

instance IsLabel "wyyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 w y y y

instance IsLabel "wyyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w y y z

instance IsLabel "wyyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 w y y w

instance IsLabel "wyzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 w y z x

instance IsLabel "wyzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w y z y

instance IsLabel "wyzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w y z z

instance IsLabel "wyzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w y z w

instance IsLabel "wywx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w y w x

instance IsLabel "wywy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 w y w y

instance IsLabel "wywz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w y w z

instance IsLabel "wyww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 w y w w

instance IsLabel "wzxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w z x x

instance IsLabel "wzxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 w z x y

instance IsLabel "wzxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w z x z

instance IsLabel "wzxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w z x w

instance IsLabel "wzyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y z w -> vec4 w z y x

instance IsLabel "wzyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w z y y

instance IsLabel "wzyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w z y z

instance IsLabel "wzyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w z y w

instance IsLabel "wzzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w z z x

instance IsLabel "wzzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w z z y

instance IsLabel "wzzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 w z z z

instance IsLabel "wzzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 w z z w

instance IsLabel "wzwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w z w x

instance IsLabel "wzwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w z w y

instance IsLabel "wzwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 w z w z

instance IsLabel "wzww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 w z w w

instance IsLabel "wwxx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 w w x x

instance IsLabel "wwxy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w w x y

instance IsLabel "wwxz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w w x z

instance IsLabel "wwxw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 w w x w

instance IsLabel "wwyx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x y _ w -> vec4 w w y x

instance IsLabel "wwyy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 w w y y

instance IsLabel "wwyz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w w y z

instance IsLabel "wwyw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 w w y w

instance IsLabel "wwzx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ z w -> vec4 w w z x

instance IsLabel "wwzy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y z w -> vec4 w w z y

instance IsLabel "wwzz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 w w z z

instance IsLabel "wwzw" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 w w z w

instance IsLabel "wwwx" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \x _ _ w -> vec4 w w w x

instance IsLabel "wwwy" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ y _ w -> vec4 w w w y

instance IsLabel "wwwz" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ z w -> vec4 w w w z

instance IsLabel "wwww" (Swizzle Vec4 Vec4) where
  fromLabel = Swizzle \v -> withVec4 v \_ _ _ w -> vec4 w w w w
