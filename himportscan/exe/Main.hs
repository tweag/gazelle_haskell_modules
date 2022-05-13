{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import Data.Maybe (catMaybes)
import qualified GHC.Paths
import qualified HImportScan.GHC as GHC
import HImportScan.ImportScanner (ScannedImports, scanImportsFromFile)

main :: IO ()
main = do
  haskellSrcs <- lines <$> getContents
  -- silently ignore missing files
  -- TODO: it would be better to instead report them to the user
  dynFlags <- GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags
  printImports . catMaybes =<< mapM (scanImportsFromFile dynFlags) haskellSrcs

printImports :: [ScannedImports] -> IO ()
printImports = ByteString.Lazy.putStrLn . Aeson.encode
