{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import HImportScan.ImportScanner (ScannedImports, scanImportsFromFile)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  haskellSrcs <- lines <$> getContents
  -- silently ignore missing files
  -- TODO: it would be better to instead report them to the user
  printImports . catMaybes =<< mapM scanImportsFromFile haskellSrcs

printImports :: [ScannedImports] -> IO ()
printImports = ByteString.Lazy.putStrLn . Aeson.encode
