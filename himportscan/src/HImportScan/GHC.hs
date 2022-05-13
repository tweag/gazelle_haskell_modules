-- | A module abstracting the provenance of GHC API names
module HImportScan.GHC(module X) where

import DynFlags as X (DynFlags, xopt_set, xopt_unset)
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
    )
  )
import HeaderInfo as X (getOptions, getImports)
import HscTypes as X (mkSrcErr)
import Lexer as X
  ( ParseResult(..)
  , Token(..)
  , lexer
  , loc
  , mkParserFlags'
  , mkPStatePure, unP
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
