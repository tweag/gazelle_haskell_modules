{-# LANGUAGE PackageImports #-}
module PackageB.Other.B where

import Data.Void.Unsafe()
import "package-a" PackageA.Exposed.A()
import qualified "package-a-v2" PackageA.Exposed.A()
