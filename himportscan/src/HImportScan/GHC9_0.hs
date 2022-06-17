{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 900

-- | A module abstracting the provenance of GHC API names
module HImportScan.GHC9_0 (module X, imports, handleParseError) where

import HImportScan.GHC.FakeSettings9_0 as X

import GHC.Driver.Session as X (DynFlags, defaultDynFlags, xopt_set, xopt_unset)
import GHC.Data.EnumSet as X (empty, fromList)
import GHC.Utils.Error as X (printBagOfErrors)
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

initOpts :: DynFlags -> ParserOpts
initOpts = initParserOpts

imports dynFlagsWithExtensions sb filePath =
  let implicitPrelude =
        not $
          any ((`elem` ["-XNoImplicitPrelude"]) . GHC.unLoc)
            (GHC.getOptions dynFlagsWithExtensions sb filePath)
  in
  GHC.getImports (GHC.initOpts dynFlagsWithExtensions) implicitPrelude sb filePath

handleParseError dynFlagsWithExtensions err = do
  logger <- initLogger
  let errEnvelope = GHC.pprError <$> err
  GHC.printBagOfErrors logger dynFlagsWithExtensions errEnvelope
  throwIO (GHC.mkSrcErr errEnvelope)

#else

module HImportScan.GHC9_0 where

#endif
