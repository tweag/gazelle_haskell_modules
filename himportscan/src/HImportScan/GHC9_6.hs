{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 906

-- | A module abstracting the provenance of GHC API names
module HImportScan.GHC9_6 (module X, imports, handleParseError, getOptions) where

import HImportScan.GHC.FakeSettings9_6 as X

import GHC.Driver.Session as X (DynFlags, defaultDynFlags, xopt_set, xopt_unset)
import GHC.Data.EnumSet as X (empty, fromList)
import GHC.Driver.Errors as X (printMessages)
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
import GHC.Parser.Header as X (getImports)
import GHC.Types.Error (NoDiagnosticOpts (..))
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
import GHC.Driver.Config.Parser
import GHC.Utils.Logger as X
import Control.Exception (throwIO)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Driver.Errors.Types (GhcMessage(GhcPsMessage))
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Types.Error (Messages)
import GHC.Types.PkgQual (RawPkgQual (RawPkgQual, NoRawPkgQual))
import qualified GHC.Types.SourceText as StringLiteral (sl_fs)
import qualified GHC.Parser.Header as PH (getOptions)

initOpts :: DynFlags -> ParserOpts
initOpts = initParserOpts

getOptions :: DynFlags -> StringBuffer -> FilePath -> [Located String]
getOptions dynFlags sb filePath =
  snd $ PH.getOptions (initOpts dynFlags) sb filePath

imports ::
  DynFlags ->
  StringBuffer ->
  FilePath ->
  IO
    ( Either
      -- (Bag PsError)
      (Messages PsMessage)
      ( [(Maybe FastString, Located ModuleName)],
        [(Maybe FastString, Located ModuleName)], Located ModuleName
      )
    )
imports dynFlagsWithExtensions sb filePath = do
  -- [GG] We should never care about the Prelude import,
  -- since it is always a module from an external library.
  -- Hence the `False`.
  imports' <- getImports (initOpts dynFlagsWithExtensions) False sb filePath filePath

  return $ (\ (m1, m2, _, mname) -> (toFastMessage <$> m1, toFastMessage <$> m2, mname)) `fmap` imports'
  where
    toFastMessage (NoRawPkgQual, b) = (Nothing, b)
    toFastMessage (RawPkgQual stringLit, b) = (Just $ StringLiteral.sl_fs stringLit, b)

handleParseError :: DynFlags -> Messages PsMessage -> IO a
handleParseError dynFlagsWithExtensions err = do
  logger <- initLogger
  let diagOpts = initDiagOpts dynFlagsWithExtensions
      ghcErrors = GhcPsMessage <$> err
  printMessages logger NoDiagnosticOpts diagOpts err
  throwIO (mkSrcErr ghcErrors)

#else

module HImportScan.GHC9_6 where

#endif
