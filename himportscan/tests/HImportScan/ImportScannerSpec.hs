{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module HImportScan.ImportScannerSpec where

import Data.Char (isSpace)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.QQ (s)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified GHC.Paths
import qualified HImportScan.GHC as GHC
import HImportScan.ImportScanner (ModuleImport(..), ScannedImports(..), scanImports)
import Test.Hspec


-- | This type provides some pretty printing for reporting
-- test failures.
newtype NicelyPrinted = NicelyPrinted ScannedImports
  deriving Eq

instance Show NicelyPrinted where
  show (NicelyPrinted si) = Text.unpack $ showScannedImports si

showScannedImports :: ScannedImports -> Text
showScannedImports si = Text.unlines $ map ("    " <>) $
    [ ""
    , filePath si
    , moduleName si
    ] ++
    map (("  " <>) . showImport) (Set.toList $ importedModules si) ++
    [ "usesTH = " <> Text.pack (show $ usesTH si)
    ]
  where
    showImport (ModuleImport (Just pkg) x) = Text.pack (show pkg) <> " " <> x
    showImport (ModuleImport Nothing x) = x

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

testSource :: GHC.DynFlags -> Text -> Set ModuleImport -> Bool -> Text -> IO ()
testSource = testSourceWithFile "dummy.hs"

testSourceWithFile :: FilePath -> GHC.DynFlags -> Text -> Set ModuleImport -> Bool -> Text -> IO ()
testSourceWithFile file dynFlags moduleName importedModules usesTH contents = do
    fmap NicelyPrinted (scanImports dynFlags file $ stripIndentation contents)
      `shouldReturn` NicelyPrinted ScannedImports
        { filePath = Text.pack file
        , moduleName
        , importedModules
        , usesTH
        }

spec_scanImports :: Spec
spec_scanImports = beforeAll (GHC.runGhc (Just GHC.Paths.libdir) GHC.getSessionDynFlags) $ do
    let m = ModuleImport Nothing
    it "should accept empty files" $ \dynFlags ->
      testSource dynFlags "Main" [] False ""
    it "should find an import" $ \dynFlags ->
      testSource dynFlags "Main" [m "A.B.C"] False "import A.B.C"
    it "should find multiple imports" $ \dynFlags ->
      testSource dynFlags "Main" [m "A.B.C", m "A.B.D"] False [s|
         import A.B.C
         import A.B.D
      |]
    it "should accept module headers" $ \dynFlags ->
      testSource dynFlags "M" [m "A.B.C", m "A.B.D"] False [s|
         module M where

         import A.B.C
         import A.B.D
      |]
    it "should accept post fix qualified imports" $ \dynFlags ->
      testSource dynFlags "M" [m "A.B.C", m "A.B.D", m "A.B.E"] False [s|
         module M where

         import A.B.C
         import A.B.D qualified
         import A.B.E
        |]
    it "should accept package imports" $ \dynFlags ->
      testSource
        dynFlags
        "M"
        [ ModuleImport (Just "package-a") "A.B.C"
        , ModuleImport (Just "package-b") "A.B.D"
        ]
        False
        [s|
           module M where

           import "package-a" A.B.C
           import qualified "package-b" A.B.D
         |]
    it "should accept declarations after imports" $ \dynFlags ->
      testSource dynFlags "M" [m "A.B.C", m "A.B.D"] False [s|
         module M where

         import A.B.C
         import A.B.D

         f = 1
      |]
    it "should skip CPP directives" $ \dynFlags -> do
      testSource dynFlags "M" [m "A.B.C", m "A.B.D"] False [s|
         module M where

         #define a_multiline macro \
                    a second line of the macro
         import A.B.C
         import A.B.D

         f = 1
      |]
    it "should skip comments" $ \dynFlags -> do
      testSource dynFlags "M" [m "A.B.C", m "A.B.D"] False [s|
         -- | Some module
         module M where

         import A.B.C {- a comment -}
         import {- yet another comment -} A.B.D
           -- another comment

         f = 1
       |]
    it "should recognize TemplateHaskell in language pragmas" $ \dynFlags -> do
      testSource dynFlags "M" [m "A.B.C", m "A.B.D"] True [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE TemplateHaskell #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should recognize TemplateHaskell in language pragmas with multiple extensions" $ \dynFlags -> do
      testSource dynFlags "M" [m "A.B.C", m "A.B.D"] True [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeApplications #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should recognize TemplateHaskell when QuasiQuotes is used" $ \dynFlags -> do
      testSource dynFlags "M" [m "A.B.C", m "A.B.D"] True [s|
         {-# LANGUAGE CPP #-}
         {-# LANGUAGE QuasiQuotes #-}
         module M where

         import A.B.C
         import A.B.D

         f = 1
       |]
    it "should accept literate Haskell" $ \dynFlags -> do
      testSourceWithFile "dummy.lhs" dynFlags "M" [m "A.B.C", m "A.B.D"] False [s|
         > module M where
         >
         > import A.B.C
         > import A.B.D
         >
         > f = 1
       |]
