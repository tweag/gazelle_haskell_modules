{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module HImportScan.ImportScannerSpec where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.String.QQ (s)
import Test.Hspec
import HImportScan.ImportScanner (ModuleImport(..), ScannedImports(..), scanImports)
import Data.Text (Text)
import qualified Data.Text as Text


-- | This type provides some pretty printing for reporting
-- test failures.
newtype NicelyPrinted = NicelyPrinted ScannedImports
  deriving Eq

instance Show NicelyPrinted where
  show (NicelyPrinted si) = Text.unpack $ showScannedImports si

showScannedImports :: ScannedImports -> Text
showScannedImports ScannedImports{filePath, moduleName, importedModules, usesTH} =
    Text.unlines $ map ("    " <>) $
    [ ""
    , filePath
    , moduleName
    ] ++
    map (("  " <>) . showImport) (importedModules) ++
    [ "usesTH = " <> Text.pack (show usesTH)
    ]

showImport :: ModuleImport -> Text
showImport ModuleImport{hasSourcePragma, maybePackageName, moduleName} =
  let maybeSourcePragma = if hasSourcePragma then Just ":SOURCE" else Nothing
   in Text.unwords $ catMaybes [maybeSourcePragma, maybePackageName, Just moduleName]

-- |
--
-- > stripIndentation " a\n  b\n c" == "a\n b\nc"
-- > stripIndentation "a\n  b\n c" == "a\n  b\n c"
--
stripIndentation :: Text -> Text
stripIndentation t =
    let textLines = Text.lines t
        minIndentation = minimum $ map indentation $ "" : textLines
     in Text.unlines $ map (Text.drop minIndentation) textLines
  where
    indentation line =
      let (spaces, rest) = Text.span isSpace line
       in if Text.length rest == 0 then maxBound else Text.length spaces

testSource :: Text -> [ModuleImport] -> Bool -> Text -> IO ()
testSource = testSourceWithFile "dummy.hs"

testSourceWithFile :: FilePath -> Text -> [ModuleImport] -> Bool -> Text -> IO ()
testSourceWithFile file moduleName importedModules usesTH contents = do
    NicelyPrinted (scanImports file $ stripIndentation contents)
      `shouldBe` NicelyPrinted ScannedImports
        { filePath = Text.pack file
        , moduleName
        , importedModules
        , usesTH
        }

spec_scanImports :: Spec
spec_scanImports = do
    let m = ModuleImport False Nothing
    it "should accept empty files" $
      testSource "Main" [] False ""
    it "should find an import" $
      testSource "Main" [m "A.B.C"] False "import A.B.C"
    it "should find multiple imports" $
      testSource "Main" [m "A.B.C", m "A.B.D"] False [s|
         import A.B.C
         import A.B.D
      |]
    it "should accept module headers" $
      testSource "M" [m "A.B.C", m "A.B.D"] False [s|
         module M where

         import A.B.C
         import A.B.D
      |]
    it "should accept package imports" $
      testSource
        "M"
        [ ModuleImport False (Just "package-a") "A.B.C"
        , ModuleImport False (Just "package-b") "A.B.D"
        ]
        False
        [s|
           module M where

           import "package-a" A.B.C
           import qualified "package-b" A.B.D
         |]
    it "should accept source pragmas" $
      testSource
        "M"
        [ m "A.B.C"
        , ModuleImport True Nothing "A.B.D"
        ]
        False
        [s|
           module M where

           import A.B.C
           import {-# SOURCE #-} A.B.D
         |]
    it "should accept declarations after imports" $
      testSource "M" [m "A.B.C", m "A.B.D"] False [s|
         module M where

         import A.B.C
         import A.B.D

         f = 1
      |]
    it "should skip CPP directives" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] False [s|
         module M where

         #define a_multiline macro \
                    a second line of the macro
         import A.B.C
         import A.B.D

         f = 1
      |]
    it "should skip comments" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] False [s|
         -- | Some module
         module M where

         import A.B.C {- a comment -}
         import {- yet another comment -} A.B.D
           -- another comment

         f = 1
       |]
    it "should recognize TemplateHaskell in language pragmas" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] True [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE TemplateHaskell #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should recognize TemplateHaskell in language pragmas with multiple extensions" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] True [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeApplications #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should recognize TemplateHaskell when QuasiQuotes is used" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] True [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE QuasiQuotes #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should accept literate Haskell" $ do
      testSourceWithFile "dummy.lhs" "M" [m "A.B.C", m "A.B.D"] False [s|
         > module M where
         >
         > import A.B.C
         > import A.B.D
         >
         > f = 1
       |]
