{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 902

-- | A module abstracting the provenance of GHC API names
module HImportScan.GHC9_2 (module X, imports, handleParseError) where

import HImportScan.GHC.FakeSettings9_2 as X

import GHC.Driver.Session as X (DynFlags, defaultDynFlags, xopt_set, xopt_unset)
import GHC.Data.EnumSet as X (empty, fromList)
import GHC.Driver.Errors as X (printBagOfErrors)
import GHC.Data.FastString as X (FastString, mkFastString, unpackFS)
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
import GHC.Types.SourceError (mkSrcErr)
import GHC.Parser.Lexer as X
  ( ParseResult(..)
  , ParserOpts
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
import GHC.Driver.Config
import GHC.Utils.Logger as X
import GHC.Parser.Errors.Ppr (pprError)
import Control.Exception (throwIO)
import GHC.Data.Bag (Bag)
import GHC.Parser.Errors (PsError)

initOpts :: DynFlags -> ParserOpts
initOpts = initParserOpts

imports ::
  DynFlags ->
  StringBuffer ->
  FilePath ->
  IO
    ( Either
      (Bag PsError)
      ( [(Maybe FastString, Located ModuleName)],
        [(Maybe FastString, Located ModuleName)], Located ModuleName
      )
    )
imports dynFlagsWithExtensions sb filePath =
  -- [GG] We should never care about the Prelude import,
  -- since it is always a module from an external library.
  -- Hence the `False`.
  getImports (initOpts dynFlagsWithExtensions) False sb filePath filePath

handleParseError :: DynFlags -> Bag PsError -> IO a
handleParseError dynFlagsWithExtensions err = do
  logger <- initLogger
  let errEnvelope = pprError <$> err
  printBagOfErrors logger dynFlagsWithExtensions errEnvelope
  throwIO (mkSrcErr errEnvelope)

#else

module HImportScan.GHC9_2 where

#endif
