module HImportScan.GHC.Utils
  ( moduleNameToText
  , dynFlags
  ) where

import qualified HImportScan.GHC as GHC

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Module as GHC.Module
import DynFlags (DynFlags(..))
import qualified DynFlags as GHC.DynFlags
import HImportScan.GHC.Settings (fakeSettings, fakeLlvmConfig)
import qualified GHC.LanguageExtensions as GHC

moduleNameToText :: GHC.Located GHC.Module.ModuleName -> Text
moduleNameToText = Text.pack . GHC.Module.moduleNameString . GHC.unLoc

dynFlags :: DynFlags
dynFlags =
  setExtension GHC.ImportQualifiedPost $
  setExtension GHC.PackageImports $
  setExtension GHC.TemplateHaskell $
  -- we get Prelude listed if we don't unset ImplicitPrelude
  unsetExtension GHC.ImplicitPrelude $
  GHC.DynFlags.defaultDynFlags fakeSettings fakeLlvmConfig
  where
    setExtension = flip GHC.DynFlags.xopt_set
    unsetExtension = flip GHC.DynFlags.xopt_unset
