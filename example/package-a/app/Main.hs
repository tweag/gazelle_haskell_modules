{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Main where

#define a_multiline macro \
  a second line of the macro
import NonModulesLib (nonModulesLib)
import PackageA.Other.C(type DataC, DataC(DataC), pattern PatTrue)

main :: IO ()
main = do
  let
    dataC :: DataC
    dataC = DataC PatTrue
  putStrLn $ "Hello, Haskell! " ++ show nonModulesLib ++ show dataC
