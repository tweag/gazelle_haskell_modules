-- Copyright (c) 2020, Shayne Fletcher. All rights reserved.
-- SPDX-License-Identifier: BSD-3-Clause.

{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- This file is a single code path copied over from https://hackage.haskell.org/package/ghc-lib-parser-ex-8.10.0.24/docs/src/Language.Haskell.GhclibParserEx.GHC.Settings.Config.html
-- TODO[GL]: We can get rid of this file once we only support >=9.2, as ParserOpts are much smaller there.
module HImportScan.GHC.Settings(
    fakeSettings
  , fakeLlvmConfig
  )
where

import Config
import DynFlags
import Fingerprint
import GHC.Platform
import ToolSettings

fakeSettings :: Settings
fakeSettings = Settings
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sPlatformConstants=platformConstants
  , sToolSettings=toolSettings
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
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
      , platformUnregisterised=True
      }
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
