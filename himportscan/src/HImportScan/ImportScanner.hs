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
  , scanImportsFromFile
  ) where

import Control.Exception (evaluate)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Char (isAlphaNum, isSpace, toLower)
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
  , usesTH :: Bool  -- ^ Whether the module needs TH or the interpreter
  }

instance Aeson.ToJSON ScannedImports where
  toJSON (ScannedImports filePath moduleName importedModules usesTH) =
    Aeson.object
      [ ("filePath", Aeson.String filePath)
      , ("moduleName", Aeson.String moduleName)
      , ("importedModules", Aeson.toJSON importedModules)
      , ("usesTH", Aeson.toJSON usesTH)
      ]

-- | Retrieves the names of modules imported in the given
-- source file. Runs the GHC lexer only as far as necessary to retrieve
-- all of the import declarations.
scanImportsFromFile :: FilePath -> IO ScannedImports
scanImportsFromFile filePath = withFile filePath ReadMode $ \h ->
  (scanImports filePath <$> hGetContents h) >>= evaluate

scanImports :: FilePath -> String -> ScannedImports
scanImports filePath contents =
  let sbuffer = stringToStringBuffer (preprocessContents contents)
      loc = mkRealSrcLoc (mkFastString filePath) 1 1
   in case scanTokenStream filePath $ lexTokenStream sbuffer loc of
    Left err -> error err
    Right ScannedData{moduleName, importedModules, usesTH} ->
      ScannedImports
        { filePath = Text.pack filePath
        , moduleName
        , importedModules
        , usesTH
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
    , usesTH :: Bool
    }

scanTokenStream :: FilePath -> [Located Token] -> Either String ScannedData
scanTokenStream fp toks =
  case parse parser fp toks of
    Left e -> Left (show e)
    Right a -> Right a
  where
    parser = do
      langExts <- concat <$> many parseLanguagePragma
      modName <- parseModuleHeader <|> return "Main"
      _ <- optional $ satisfy "virtual brace" $ \case ITvocurly -> Just (); _ -> Nothing
      skipMany comment
      imports <- many parseImport
      return ScannedData
        { moduleName = modName
        , importedModules = nub imports
        , usesTH = any (`elem` ["TemplateHaskell", "QuasiQuotes"]) langExts
        }

    parseLanguagePragma :: Parsec [Located Token] () [String]
    parseLanguagePragma = do
      satisfyEvenComments "LANGUAGE pragma" $ \case
        ITblockComment s -> Just (getLanguageExtensionsMaybe s)
        ITlineComment _ -> Just []
        _ -> Nothing

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
    comment =
      satisfyEvenComments "comment" (\case
        ITblockComment c -> Just c
        ITlineComment c -> Just c
        _ -> Nothing
      ) <* optional (satisfyEvenComments ";" $ \case ITsemi -> Just (); _ -> Nothing)

    locToSourcePos :: Located a -> SourcePos
    locToSourcePos loc =
      let srcSpan = getLoc loc
       in case srcSpanStart srcSpan of
            RealSrcLoc realSrcLoc ->
              newPos fp (srcLocLine realSrcLoc) (srcLocCol realSrcLoc)
            _ ->
              newPos fp 0 0

getLanguageExtensionsMaybe :: String -> [String]
getLanguageExtensionsMaybe = \case
    '{':'-':'#':s0 ->
      case dropWhile isSpace s0 of
        'L':'A':'N':'G':'U':'A':'G':'E':x:s1 | isSpace x ->
          readLanguageExtensions [] s1
        _ ->
          []
    _ ->
      []
  where
    readLanguageExtensions acc s =
      case takeLanguageExtension s of
        (e, rest) | not (null e) -> readLanguageExtensions (e : acc) rest
        _ -> acc

    takeLanguageExtension s =
      span isAlphaNum $
      dropWhile (\x -> isSpace x || x == ',') s

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
