module PackageC.Exposed.A where

import PackageC.Other.B()

f :: forall a. a -> a
f x = x
