{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module HImportScan.ImportScanner
  ( ScannedImports(..)
  , ModuleImport(..)
  , ImportMethod(..)
  , scanImports
  , scanImportsFromFile
  ) where

import qualified Text.JSON as Json
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (doesFileExist)

#if __GLASGOW_HASKELL__ >= 902
import HImportScan.GHC9_2 as GHC
#elif __GLASGOW_HASKELL__ == 900
import HImportScan.GHC9_0 as GHC
#else
import HImportScan.GHC8_10 as GHC
#endif

-- | Holds the names of modules imported in a Haskell module.
data ScannedImports = ScannedImports
  { filePath :: String  -- ^ Path of the Haskell module
  , moduleName :: String  -- ^ The module name
  , importedModules :: Set ModuleImport -- ^ The modules imported in this module
  , usesTH :: Bool  -- ^ Whether the module needs TH or the interpreter
  , isBoot :: Bool -- ^ Whether the module is a boot module
  }
  deriving Eq

-- | A module import holds a module name and an optional package name
-- when using package imports.
-- It also stores if the import was a normal or a source import.
data ModuleImport = ModuleImport
  { isSourceImported :: ImportMethod
  , packageName :: Maybe String
  , moduleName :: String
  }
  deriving (Eq, Ord)

data ImportMethod = SourceImport | NormalImport
  deriving (Eq, Ord)

jsonString :: String -> Json.JSValue
jsonString = Json.JSString . Json.toJSString

instance Json.JSON ScannedImports where
  showJSON ScannedImports{..} = Json.JSObject $
    Json.toJSObject $
      [ ("filePath", jsonString filePath)
      , ("moduleName", jsonString moduleName)
      , ("importedModules", Json.showJSON importedModules)
      ] ++
      [ ("usesTH", Json.JSBool True) | usesTH] ++
      [ ("isBoot", Json.JSBool True) | isBoot]

  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

instance Json.JSON ModuleImport where
  showJSON (ModuleImport importMethod maybePackageName moduleName) = Json.JSObject $
    Json.toJSObject $
      -- Here we rely on the default value in Golang for booleans (false)
      -- to avoid storing a boolean in the JSON file whenever one is not doing a source import.
      [("isSourceImported", Json.JSBool True) | importMethod == SourceImport] ++
      -- Here again, the default Golang value for string is "",
      -- hence we do not write this field in case it is not necessary.
      packageNameField ++
      [ ("moduleName", jsonString moduleName) ]
    where
      packageNameField =
        case maybePackageName of
          Nothing -> []
          Just s -> [ ("packageName", jsonString s) ]

  readJSON _ = error "We are only exporting the result to JSON, hence the read function is undefined."

-- | Retrieves the names of modules imported in the given
-- source file. Runs the GHC lexer only as far as necessary to retrieve
-- all of the import declarations.
-- If a file is missing, we return 'Nothing'.
-- TODO: It would be better to give more information on the missing file,
-- to report to the user.
scanImportsFromFile :: FilePath -> IO (Maybe ScannedImports)
scanImportsFromFile filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
  then fmap Just . scanImports filePath =<< readFile filePath
  else pure Nothing

-- TODO[GL]: This function is only in IO because
-- * we use printBagOfErrors to report an error, but we can easily factor that out
-- * getImports is in IO, which in turn is only in IO to throw an error
--   Perhaps we could raise an issue at ghc to make a pure variant.
scanImports :: FilePath -> String -> IO ScannedImports
scanImports filePath contents = do
  let sb = GHC.stringToStringBuffer $ preprocessContents contents

  -- TODO[GL]: Once we're on ghc 9.2 we can get rid of all the things relating to dynFlags, and use the much smaller
  -- ParserOpts, as getImports no longer depends on DynFlags then.
  let dynFlagsWithExtensions = toggleDynFlags $ GHC.defaultDynFlags GHC.fakeSettings GHC.fakeLlvmConfig

  let
    -- [GL] The fact that the resulting strings here contain the "-X"s makes me a bit doubtful that this is the right approach,
    -- but this is what I found for now.
    usesTH =
      any ((`elem` ["-XTemplateHaskell", "-XQuasiQuotes"]) . GHC.unLoc)
        (GHC.getOptions dynFlagsWithExtensions sb filePath)
    isBoot = ".hs-boot" `isSuffixOf` filePath
  GHC.imports dynFlagsWithExtensions sb filePath >>= \case
    -- It's important that we error in this case to signal to the user that
    -- something needs fixing in the source file.
    Left err -> GHC.handleParseError dynFlagsWithExtensions err
    Right (sourceImports, normalImports, moduleName) -> do
      pure ScannedImports
            { filePath = filePath
            , moduleName = moduleNameToString moduleName
            , importedModules =
                let
                  toModuleImport :: (ImportMethod, (Maybe GHC.FastString, GHC.Located GHC.ModuleName)) -> ModuleImport
                  toModuleImport (importMethod, (mfs, locatedModuleName)) =
                    ModuleImport
                      importMethod
                      (fmap GHC.unpackFS mfs)
                      (moduleNameToString locatedModuleName)
                 in Set.fromList $ map toModuleImport $ map (SourceImport,) sourceImports ++ map (NormalImport,) normalImports
            , usesTH
            , isBoot
            }
  where
    preprocessContents = unlines . flipBirdTracks filePath . clearCPPDirectives . lines

    moduleNameToString = GHC.moduleNameString . GHC.unLoc

-- Toggle extensions to the state we want them in.
-- We should handle all forms of imports.
-- We turn off ImplicitPrelude, because otherwise it shows up in imports lists which ghc returns.
toggleDynFlags :: GHC.DynFlags -> GHC.DynFlags
toggleDynFlags dflags0 =
  let dflags1 = foldl GHC.xopt_set dflags0
                  [ GHC.ImportQualifiedPost
                  , GHC.PackageImports
                  , GHC.TemplateHaskell
                  , GHC.PatternSynonyms
                  , GHC.ExplicitNamespaces
                  , GHC.MagicHash
                  ]
   in GHC.xopt_unset dflags1 GHC.ImplicitPrelude

-- | Clear CPP directives since they would otherwise confuse the scanner.
--
-- Takes as inputs the contents of a Haskell source file and replaces all
-- lines starting with a hash ('#') with an empty line.
--
-- Honours multiline directives (\-terminated) too
--
clearCPPDirectives :: [String] -> [String]
clearCPPDirectives = \case
  xs@(('#':_) : _) ->
    let (nlines, rest) = dropDirectiveLines xs
     in replicate nlines "" ++ clearCPPDirectives rest
  (xs : xss) -> xs : clearCPPDirectives xss
  [] -> []
  where
    dropDirectiveLines xs =
      let (directive, rest) = span endsWithBackslash xs
       in (length directive + 1, drop 1 rest)

    endsWithBackslash = isSuffixOf "\\"

-- | The start of bird tracks are replaced with spaces, and the
-- comment lines are replaced with empty lines as long as the given
-- file has .lhs extension.
flipBirdTracks :: FilePath -> [String] -> [String]
flipBirdTracks f =
    if isSuffixOf ".lhs" (map toLower f) then map flipBirdTrack
      else id
  where
    flipBirdTrack :: String -> String
    flipBirdTrack ('>' : xs) = " " <> xs
    flipBirdTrack _ = " "
