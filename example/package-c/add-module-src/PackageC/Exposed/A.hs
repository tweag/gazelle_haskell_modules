module PackageC.Exposed.A where

import PackageC.Other.B()
import PackageC.Exposed.NewModule()

f :: forall a. a -> a
f x = x
