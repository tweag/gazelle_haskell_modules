{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 900

-- | A module abstracting the provenance of GHC API names
module HImportScan.GHC9_0 (module X, imports, handleParseError) where

import HImportScan.GHC.FakeSettings9_0 as X

import Control.Exception (throwIO)
import GHC.Driver.Session as X (DynFlags, defaultDynFlags, xopt_set, xopt_unset)
import GHC.Data.EnumSet as X (empty, fromList)
import GHC.Utils.Error as X (printBagOfErrors, ErrMsg)
import GHC.Data.FastString as X (FastString, mkFastString, bytesFS)
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
import GHC.Parser.Header as X (getOptions, getImports)
import GHC.Driver.Types as X (mkSrcErr)
import GHC.Parser.Lexer as X
  ( ParseResult(..)
  , Token(..)
  , lexer
  , loc
  , unP
  )
import GHC.Unit.Module as X (ModuleName, moduleNameString)
import GHC.Types.SrcLoc as X
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
import GHC.Data.StringBuffer as X (StringBuffer(StringBuffer), stringToStringBuffer)
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

module HImportScan.GHC9_0 where

#endif
