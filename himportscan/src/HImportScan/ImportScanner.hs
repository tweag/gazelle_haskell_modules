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

module HImportScan.ImportScanner
  ( ScannedImports(..)
  , ModuleImport(..)
  , scanImports
  , scanImportsFromFile
  ) where

import Control.Exception (throwIO)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import HImportScan.GHC as GHC
import qualified HImportScan.GHC.Utils as GHC.Utils
import System.Directory (doesFileExist)

-- | Holds the names of modules imported in a Haskell module.
data ScannedImports = ScannedImports
  { filePath :: Text  -- ^ Path of the Haskell module
  , moduleName :: Text  -- ^ The module name
  , importedModules :: Set ModuleImport -- ^ The modules imported in this module
  , usesTH :: Bool  -- ^ Whether the module needs TH or the interpreter
  }
  deriving Eq

-- | A module import holds a module name and an optional package name
-- when using package imports.
data ModuleImport = ModuleImport (Maybe Text) Text
  deriving (Eq, Ord)

instance Aeson.ToJSON ScannedImports where
  toJSON (ScannedImports filePath moduleName importedModules usesTH) =
    Aeson.object
      [ ("filePath", Aeson.String filePath)
      , ("moduleName", Aeson.String moduleName)
      , ("importedModules", Aeson.toJSON importedModules)
      , ("usesTH", Aeson.toJSON usesTH)
      ]

instance Aeson.ToJSON ModuleImport where
  toJSON (ModuleImport maybePackageName moduleName) =
    Aeson.toJSON $ catMaybes [maybePackageName, Just moduleName]

-- | Retrieves the names of modules imported in the given
-- source file. Runs the GHC lexer only as far as necessary to retrieve
-- all of the import declarations.
-- If a file is missing, we return 'Nothing'.
-- TODO: It would be better to give more information on the missing file,
-- to report to the user.
scanImportsFromFile :: GHC.DynFlags -> FilePath -> IO (Maybe ScannedImports)
scanImportsFromFile dynFlags filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
  then fmap Just . scanImports dynFlags filePath =<< Text.readFile filePath
  else pure Nothing

-- TODO[GL]: This function is only in IO because
-- * we use printBagOfErrors to report an error, but we can easily factor that out
-- * getImports is in IO, which in turn is only in IO to throw an error
--   Perhaps we could raise an issue at ghc to make a pure variant.
scanImports :: GHC.DynFlags -> FilePath -> Text -> IO ScannedImports
scanImports dynFlags filePath contents = do
  -- TODO[GL]: going through String just because StringBuffer doesn't have a Text interface is not the best
  -- we could potentially skip that with more effort (e.g. go through ByteString, which is very similar to a StringBuffer)
  let sb = GHC.stringToStringBuffer $ Text.unpack $ preprocessContents contents

  -- TODO[GL]: Once we're on ghc 9.2 we can get rid of all the things relating to dynFlags, and use the much smaller
  -- ParserOpts, as getImports no longer depends on DynFlags then.
  let dynFlagsWithExtensions = GHC.Utils.toggleDynFlags dynFlags

  let
    -- [GL] The fact that the resulting strings here contain the "-X"s makes me a bit doubtful that this is the right approach,
    -- but this is what I found for now.
    usesTH =
      any (`elem` ["-XTemplateHaskell", "-XQuasiQuotes"]) $
        map GHC.unLoc $
          GHC.getOptions dynFlagsWithExtensions sb filePath
  GHC.getImports dynFlagsWithExtensions sb filePath filePath >>= \case
    -- It's important that we error in this case, as otherwise the parser has gone terribly wrong probably.
    -- It's also ok to print the error, as it is usually descriptive and well formatted.
    -- The way we error here is that we're passing unexpected output to the go library.
    -- This is far from ideal, however handling this better would require being able to communicate errors better to go.
    Left err -> do
      GHC.printBagOfErrors dynFlagsWithExtensions err
      throwIO (GHC.mkSrcErr err)
    Right (sourceImports, normalImports, moduleName) -> do
      pure ScannedImports
            { filePath = Text.pack filePath
            , moduleName = GHC.Utils.moduleNameToText moduleName
            , importedModules =
                let
                  toModuleImport :: (Maybe GHC.FastString, GHC.Located GHC.ModuleName) -> ModuleImport
                  toModuleImport (mfs, locatedModuleName) =
                    ModuleImport
                      (fmap (Text.decodeUtf8 . GHC.bytesFS) mfs)
                      (GHC.Utils.moduleNameToText locatedModuleName)
                 in Set.fromList $ map toModuleImport $ sourceImports ++ normalImports
            , usesTH
            }
  where
    preprocessContents = Text.unlines . flipBirdTracks filePath . clearCPPDirectives . Text.lines

-- | Clear CPP directives since they would otherwise confuse the scanner.
--
-- Takes as inputs the contents of a Haskell source file and replaces all
-- lines starting with a hash ('#') with an empty line.
--
-- Honours multiline directives (\-terminated) too
--
clearCPPDirectives :: [Text] -> [Text]
clearCPPDirectives = \case
  xs@(t : _) | Text.isPrefixOf "#" t ->
    let (nlines, rest) = dropDirectiveLines xs
     in replicate nlines "" ++ clearCPPDirectives rest
  (xs : xss) -> xs : clearCPPDirectives xss
  [] -> []
  where
    dropDirectiveLines xs =
      let (directive, rest) = span endsWithBackslash xs
       in (length directive + 1, drop 1 rest)

    endsWithBackslash = Text.isSuffixOf "\\"

-- | The start of bird tracks are replaced with spaces, and the
-- comment lines are replaced with empty lines as long as the given
-- file has .lhs extension.
flipBirdTracks :: FilePath -> [Text] -> [Text]
flipBirdTracks f =
    if isSuffixOf ".lhs" (map toLower f) then map flipBirdTrack
      else id
  where
    flipBirdTrack :: Text -> Text
    flipBirdTrack xs | Text.isPrefixOf ">" xs = " " <> Text.drop 1 xs
    flipBirdTrack _ = " "
