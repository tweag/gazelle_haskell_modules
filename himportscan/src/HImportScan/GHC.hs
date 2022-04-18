{-# LANGUAGE CPP #-}

-- | A module abstracting the provenance of GHC API names
module HImportScan.GHC(
  module X,
  mkParserOpts,
  initParserState,
  )
where

#if __GLASGOW_HASKELL__ >= 900

import GHC.Data.EnumSet as X (empty, fromList)
import GHC.Data.FastString as X (mkFastString, unpackFS)
import GHC.Parser.Lexer as X
  ( ParseResult(..)
  , Token(..)
  , lexer
  , loc
  , unP
  )
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
import GHC.Data.StringBuffer as X (StringBuffer(StringBuffer))

#else

import EnumSet as X (empty, fromList)
import FastString as X (mkFastString, unpackFS)
import Lexer as X
  ( ParseResult(..)
  , Token(..)
  , lexer
  , loc
  , mkParserFlags'
  , mkPStatePure, unP
  )
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
import StringBuffer as X (StringBuffer(StringBuffer))
import qualified Lexer
import EnumSet (EnumSet)
import DynFlags (WarningFlag)
import GHC.LanguageExtensions.Type (Extension)

mkParserOpts
  :: EnumSet WarningFlag
  -> EnumSet Extension
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Lexer.ParserFlags
mkParserOpts a b = mkParserFlags' a b (error "lexTokenStreamUnitId")

initParserState :: Lexer.ParserFlags -> StringBuffer -> RealSrcLoc -> Lexer.PState
initParserState = mkPStatePure

#endif

#if __GLASGOW_HASKELL__ >= 902

import GHC.Parser.Lexer
  ( mkParserOpts
  , initParserState
  )

#elif __GLASGOW_HASKELL__ == 900

import GHC.Parser.Lexer
  ( mkParserFlags'
  , mkPStatePure
  )
import qualified GHC.Parser.Lexer as Lexer
import GHC.Data.EnumSet (EnumSet)
import GHC.Driver.Flags (WarningFlag)
import GHC.LanguageExtensions.Type (Extension)

mkParserOpts
  :: EnumSet WarningFlag
  -> EnumSet Extension
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Lexer.ParserFlags
mkParserOpts a b = mkParserFlags' a b (error "lexTokenStreamUnitId")

initParserState :: Lexer.ParserFlags -> StringBuffer -> RealSrcLoc -> Lexer.PState
initParserState = mkPStatePure

#endif
