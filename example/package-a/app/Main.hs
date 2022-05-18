{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MagicHash #-}
module Main where

#define a_multiline macro \
  a second line of the macro
import NonModulesLib (nonModulesLib)
import PackageA.Other.C(type DataC, DataC(DataC), pattern PatTrue)
import GHC.Exts (Int(..), Int#)

main :: IO ()
main = do
  let
    dataC :: DataC
    dataC = DataC PatTrue
    int# :: Int#
    int# = case 1 of I# i -> i
  putStrLn $ "Hello, Haskell! " ++ show nonModulesLib ++ show dataC ++ show (I# int#)
