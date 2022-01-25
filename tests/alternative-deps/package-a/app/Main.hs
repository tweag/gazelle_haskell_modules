{-# LANGUAGE CPP #-}
module Main where

#define a_multiline macro \
  a second line of the macro
import PackageA.Other.C()

main :: IO ()
main = putStrLn "Hello, Haskell!"
