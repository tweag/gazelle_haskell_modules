{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Text.JSON as Json
import Data.Maybe (catMaybes)
import HImportScan.ImportScanner (ScannedImports, scanImportsFromFile)

main :: IO ()
main = do
  haskellSrcs <- lines <$> getContents
  -- silently ignore missing files
  -- TODO: it would be better to instead report them to the user
  printImports . catMaybes =<< mapM scanImportsFromFile haskellSrcs

printImports :: [ScannedImports] -> IO ()
printImports = putStrLn . Json.encode
