-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.
{-#LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ == 900

{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- This file is a single code path copied over from https://hackage.haskell.org/package/ghc-lib-parser-ex-8.10.0.24/docs/src/Language.Haskell.GhclibParserEx.GHC.Settings.Config.html
-- TODO[GL]: We can get rid of this file once we only support >=9.2, as ParserOpts are much smaller there.
module HImportScan.GHC.FakeSettings9_0(
    fakeSettings
  , fakeLlvmConfig
  )
where

import GHC.Settings.Config
import GHC.Driver.Session
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Settings

fakeSettings :: Settings
fakeSettings = Settings
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  , sRawSettings=[]
  }
  where
    toolSettings = ToolSettings {
      toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion =
      GhcNameVersion{ghcNameVersion_programName="ghc"
                    ,ghcNameVersion_projectVersion=cProjectVersion
                    }
    platform =
      Platform{
        platformWordSize=PW8
      , platformByteOrder = LittleEndian
      , platformUnregisterised=True
      , platformHasGnuNonexecStack = False
      , platformHasIdentDirective = False
      , platformHasSubsectionsViaSymbols = False
      , platformIsCrossCompiling = False
      , platformLeadingUnderscore = False
      , platformTablesNextToCode = False
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
      }
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

#else

module HImportScan.GHC.FakeSettings9_0 where

#endif