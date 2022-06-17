{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 810

-- | A module abstracting the provenance of GHC API names
module HImportScan.GHC8_10 (module , imports, handleParseError) where

import HImportScan.GHC.FakeSettings8_10 as X

import Control.Exception (throwIO)
import DynFlags as X (DynFlags, defaultDynFlags, xopt_set, xopt_unset)
import EnumSet as X (empty, fromList)
import ErrUtils as X (printBagOfErrors)
import FastString as X (FastString, mkFastString, bytesFS)
import GHC as X (runGhc, getSessionDynFlags)
import GHC.LanguageExtensions as X
  (Extension
    ( ImportQualifiedPost
    , PackageImports
    , TemplateHaskell
    , ImplicitPrelude
    , PatternSynonyms
    , ExplicitNamespaces
    , MagicHash
    )
  )
import HeaderInfo as X (getOptions, getImports)
import HscTypes as X (mkSrcErr)
import Lexer as X
  ( ParseResult(..)
  , Token(..)
  , lexer
  , loc
  , unP
  )
import Module as X (ModuleName, moduleNameString)
import SrcLoc as X
  ( Located
  , RealSrcLoc
  , SrcLoc(RealSrcLoc)
  , getLoc
  , mkRealSrcLoc
  , srcLocLine
  , srcLocCol
  , srcSpanStart
  , unLoc
  )
import StringBuffer as X (StringBuffer(StringBuffer), stringToStringBuffer)
import GHC.Data.Bag (Bag)

imports ::
  DynFlags ->
  StringBuffer ->
  FilePath ->
  IO
    ( Either
      (Bag ErrMsg)
      ( [(Maybe FastString, Located ModuleName)],
        [(Maybe FastString, Located ModuleName)], Located ModuleName
      )
    )
imports dynFlagsWithExtensions sb filePath =
  getImports dynFlagsWithExtensions sb filePath filePath

handleParseError :: DynFlags -> Bag ErrMsg -> IO a
handleParseError dynFlagsWithExtensions err = do
  printBagOfErrors dynFlagsWithExtensions err
  throwIO (mkSrcErr err)

#else

module HImportScan.GHC8_10 where

#endif
