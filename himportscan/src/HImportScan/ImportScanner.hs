{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module HImportScan.ImportScanner
  ( ScannedImports
  , scanImports
  ) where

import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.List (isSuffixOf, nub)
import Data.Text (Text)
import qualified Data.Text as Text
import EnumSet
import FastString
import Lexer hiding (lexTokenStream)
import SrcLoc
import StringBuffer

import System.IO (IOMode(ReadMode), hGetContents, withFile)
import Text.Parsec hiding (satisfy)
import Text.Parsec.Pos (newPos)


-- | Holds the names of modules imported in a Haskell module.
data ScannedImports = ScannedImports
  { filePath :: Text  -- ^ Path of the Haskell module
  , moduleName :: Text  -- ^ The module name
  , importedModules :: [Text] -- ^ The modules imported in this module
  }

instance Aeson.ToJSON ScannedImports where
  toJSON (ScannedImports filePath moduleName importedModules) =
    Aeson.object
      [ ("filePath", Aeson.String filePath)
      , ("moduleName", Aeson.String moduleName)
      , ("importedModules", Aeson.toJSON importedModules)
      ]

-- | Retrieves the names of modules imported in the given
-- source file. Runs the GHC lexer only as far as necessary to retrieve
-- all of the import declarations.
scanImports :: FilePath -> IO ScannedImports
scanImports filePath = withFile filePath ReadMode $ \h -> do
  contents <- hGetContents h
  let sbuffer = stringToStringBuffer (preprocessContents contents)
      loc = mkRealSrcLoc (mkFastString filePath) 1 1
  case scanTokenStream filePath $ lexTokenStream sbuffer loc of
    Left err -> error err
    Right ScannedData{moduleName, importedModules} ->
      return ScannedImports
        { filePath = Text.pack filePath
        , moduleName
        , importedModules
        }

  where
    preprocessContents = unlines . flipBirdTracks filePath . clearCPPDirectives . lines

-- | Clear CPP directives since they would otherwise confuse the scanner.
--
-- Takes as inputs the contents of a Haskell source file and replaces all
-- lines starting with a hash ('#') with an empty line.
--
-- Honours multiline directives (\-terminated) too
--
clearCPPDirectives :: [String] -> [String]
clearCPPDirectives = \case
  xs@(('#' : _) : _) ->
    let (nlines, rest) = dropDirectiveLines xs
     in replicate nlines "" ++ clearCPPDirectives rest
  (xs : xss) -> xs : clearCPPDirectives xss
  [] -> []
  where
    dropDirectiveLines xs =
      let (directive, rest) = span endsWithBackslash xs
       in (length directive + 1, drop 1 rest)

    endsWithBackslash [] = False
    endsWithBackslash xs = last xs == '\\'

-- | The start of bird tracks are replaced with spaces, and the
-- comment lines are replaced with empty lines as long as the given
-- file has .lhs extension.
flipBirdTracks :: FilePath -> [String] -> [String]
flipBirdTracks f =
    if isSuffixOf ".lhs" (map toLower f) then map flipBirdTrack
      else id
  where
    flipBirdTrack :: String -> String
    flipBirdTrack ('>' : xs) = ' ' : xs
    flipBirdTrack _ = " "

data ScannedData = ScannedData
    { moduleName :: Text
    , importedModules :: [Text]
    }

scanTokenStream :: FilePath -> [Located Token] -> Either String ScannedData
scanTokenStream fp toks =
  case parse parser fp toks of
    Left e -> Left (show e)
    Right a -> Right a
  where
    parser = do
      skipMany comment
      modName <- parseModuleHeader <|> return "Main"
      _ <- optional $ satisfy "virtual brace" $ \case ITvocurly -> Just (); _ -> Nothing
      imports <- many parseImport
      return ScannedData
        { moduleName = modName
        , importedModules = nub imports
        }

    parseModuleHeader = do
      _ <- satisfy "module" $ \case
        ITmodule -> Just ()
        _ -> Nothing
      parseModuleName <* parseHeaderTail

    parseHeaderTail = do
      skipMany $ satisfy "not where" $ \case ITwhere -> Nothing; _ -> Just ()
      satisfy "where" $ \case ITwhere -> Just (); _ -> Nothing

    parseModuleName = flip labels ["ITqconid", "ITconid"] $ do
      satisfy "a module name" $ \case
        ITqconid (q, n) -> Just $ Text.pack $ unpackFS (q <> "." <> n)
        ITconid n -> Just $ Text.pack $ unpackFS n
        _ -> Nothing

    parseImport = do
      _ <- satisfy "import" $ \case ITimport -> Just (); _ -> Nothing
      _ <- optional $ satisfy "qualified" $ \case ITqualified -> Just (); _ -> Nothing
      _ <- optional $ satisfy "string" $ \case ITstring{} -> Just (); _ -> Nothing
      parseModuleName <* parseImportTail

    parseImportTail = do
      _ <- optional $ do
        satisfy "as" $ \case ITas -> Just (); _ -> Nothing
        parseModuleName
      _ <- optional $ satisfy "hiding" $ \case IThiding -> Just (); _ -> Nothing
      _ <- optional parseNestedParens
      void $ optional $ satisfy ";" $ \case ITsemi -> Just (); _ -> Nothing

    parseNestedParens = flip label "nested parentheses" $ do
      satisfy "(" $ \case IToparen -> Just (); _ -> Nothing
      skipMany $ satisfy "not ( or )" $ \case IToparen -> Nothing; ITcparen -> Nothing; _ -> Just ()
      skipMany $ do
        parseNestedParens
        skipMany $ satisfy "not ( or )" $ \case IToparen -> Nothing; ITcparen -> Nothing; _ -> Just ()
      void $ satisfy ")" $ \case ITcparen -> Just (); _ -> Nothing

    satisfy lbl f = satisfyEvenComments lbl f <* skipMany comment

    satisfyEvenComments lbl f =
      token (show . unLoc) locToSourcePos (f . unLoc) <?> lbl

    comment :: Parsec [Located Token] () String
    comment = satisfyEvenComments "comment" $ \case
      ITblockComment c -> Just c
      ITlineComment c -> Just c
      _ -> Nothing

    locToSourcePos :: Located a -> SourcePos
    locToSourcePos loc =
      let srcSpan = getLoc loc
       in case srcSpanStart srcSpan of
            RealSrcLoc realSrcLoc ->
              newPos fp (srcLocLine realSrcLoc) (srcLocCol realSrcLoc)
            _ ->
              newPos fp 0 0

lexTokenStream :: StringBuffer -> RealSrcLoc -> [Located Token]
lexTokenStream buf loc =
  let allExtensions = [minBound..maxBound]
      parserFlags = mkParserFlags'
        EnumSet.empty
        (EnumSet.fromList allExtensions)
        (error "lexTokenStreamUnitId")
        False
        False
        True
        True
      initState = mkPStatePure parserFlags buf loc
   in go initState
  where
    go st = case unP (lexer False return) st of
      POk _st' (L _ ITeof) -> []
      POk st' tok -> tok : go st'
      PFailed st' -> error $ "Lexer error at " ++ show (Lexer.loc st')
