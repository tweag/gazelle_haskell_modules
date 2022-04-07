module PackageC.Exposed.E where

import PackageC.Other.B()

f :: forall a. a -> a
f x = x
