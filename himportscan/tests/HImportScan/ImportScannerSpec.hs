{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module HImportScan.ImportScannerSpec where

import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.QQ (s)
import Data.Text (Text)
import qualified Data.Text as Text
import HImportScan.ImportScanner (ModuleImport(..), ScannedImports(..), ImportMethod(..), scanImports)
import Test.Hspec


-- | This type provides some pretty printing for reporting
-- test failures.
newtype NicelyPrinted = NicelyPrinted ScannedImports
  deriving Eq

instance Show NicelyPrinted where
  show (NicelyPrinted si) = Text.unpack $ showScannedImports si

showScannedImports :: ScannedImports -> Text
showScannedImports ScannedImports{..} = Text.unlines $ map ("    " <>) $
    [ ""
    , filePath
    , moduleName
    ] ++
    map (("  " <>) . showImport) (Set.toList importedModules) ++
    [ "usesTH = " <> Text.pack (show usesTH)
    , "isBoot = " <> Text.pack (show isBoot)
    ]
  where
    showImport (ModuleImport importMethod (Just pkg) x) =
      Text.pack (show pkg) <> " " <> x <> showImportMethodExtension importMethod
    showImport (ModuleImport importMethod Nothing x) =
      x <> showImportMethodExtension importMethod

    showImportMethodExtension SourceImport = ".hs-boot"
    showImportMethodExtension NormalImport = ""

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

testSource :: Text -> Set ModuleImport -> Bool -> Bool -> Text -> IO ()
testSource = testSourceWithFile "dummy.hs"

testSourceWithFile :: FilePath -> Text -> Set ModuleImport -> Bool -> Bool -> Text -> IO ()
testSourceWithFile file moduleName importedModules usesTH isBoot contents = do
    fmap NicelyPrinted (scanImports file $ stripIndentation contents)
      `shouldReturn` NicelyPrinted ScannedImports
        { filePath = Text.pack file
        , moduleName
        , importedModules
        , usesTH
        , isBoot
        }

spec_scanImports :: Spec
spec_scanImports = do
    let m = ModuleImport NormalImport Nothing
    it "should accept empty files" $
      testSource "Main" [] False False ""
    it "should find an import" $
      testSource "Main" [m "A.B.C"] False False "import A.B.C"
    it "should find multiple imports" $
      testSource "Main" [m "A.B.C", m "A.B.D"] False False [s|
         import A.B.C
         import A.B.D
      |]
    it "should accept module headers" $
      testSource "M" [m "A.B.C", m "A.B.D"] False False [s|
         module M where

         import A.B.C
         import A.B.D
      |]
    it "should accept post fix qualified imports" $
      testSource "M" [m "A.B.C", m "A.B.D", m "A.B.E"] False False [s|
         module M where

         import A.B.C
         import A.B.D qualified
         import A.B.E
        |]
    it "should accept package imports" $
      testSource
        "M"
        [ ModuleImport NormalImport (Just "package-a") "A.B.C"
        , ModuleImport NormalImport (Just "package-b") "A.B.D"
        ]
        False
        False
        [s|
           module M where

           import "package-a" A.B.C
           import qualified "package-b" A.B.D
         |]
    it "should accept declarations after imports" $
      testSource "M" [m "A.B.C", m "A.B.D"] False False [s|
         module M where

         import A.B.C
         import A.B.D

         f = 1
      |]
    it "should skip CPP directives" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] False False [s|
         module M where

         #define a_multiline macro \
                    a second line of the macro
         import A.B.C
         import A.B.D

         f = 1
      |]
    it "should skip comments" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] False False [s|
         -- | Some module
         module M where

         import A.B.C {- a comment -}
         import {- yet another comment -} A.B.D
           -- another comment

         f = 1
       |]
    it "should recognize TemplateHaskell in language pragmas" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] True False [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE TemplateHaskell #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should recognize TemplateHaskell in language pragmas with multiple extensions" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] True False [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeApplications #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should recognize TemplateHaskell when QuasiQuotes is used" $ do
      testSource "M" [m "A.B.C", m "A.B.D"] True False [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE QuasiQuotes #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should accept literate Haskell" $ do
      testSourceWithFile "dummy.lhs" "M" [m "A.B.C", m "A.B.D"] False False [s|
         > module M where
         >
         > import A.B.C
         > import A.B.D
         >
         > f = 1
       |]
