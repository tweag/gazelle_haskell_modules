{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import HImportScan.ImportScanner (ScannedImports, scanImportsFromFile)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy

main :: IO ()
main = do
  haskellSrcs <- lines <$> getContents
  mapM scanImportsFromFile haskellSrcs >>= printImports

printImports :: [ScannedImports] -> IO ()
printImports = ByteString.Lazy.putStrLn . Aeson.encode
