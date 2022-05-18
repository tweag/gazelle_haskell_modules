{-# LANGUAGE PatternSynonyms #-}
module PackageA.Other.C where

import PackageA.Exposed.A()

data DataC = DataC Bool
  deriving Show

pattern PatTrue :: Bool
pattern PatTrue = True
