{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import HImportScan.ImportScanner (ScannedImports, scanImports)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy

main :: IO ()
main = do
  haskellSrcs <- lines <$> getContents
  mapM scanImports haskellSrcs >>= printImports

printImports :: [ScannedImports] -> IO ()
printImports = ByteString.Lazy.putStrLn . Aeson.encode
