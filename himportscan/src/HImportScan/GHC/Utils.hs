module HImportScan.GHC.Utils
  ( moduleNameToText
  , toggleDynFlags
  ) where

import qualified HImportScan.GHC as GHC

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Module as GHC.Module
import DynFlags (DynFlags(..))
import qualified DynFlags as GHC.DynFlags
import qualified GHC.LanguageExtensions as GHC

moduleNameToText :: GHC.Located GHC.Module.ModuleName -> Text
moduleNameToText = Text.pack . GHC.Module.moduleNameString . GHC.unLoc

-- Toggle extensions to the state we want them in.
-- We should handle all forms of imports.
-- We turn off ImplicitPrelude, because otherwise it shows up in imports lists which ghc returns.
toggleDynFlags :: DynFlags -> DynFlags
toggleDynFlags =
  setExtension GHC.ImportQualifiedPost .
  setExtension GHC.PackageImports .
  setExtension GHC.TemplateHaskell .
  unsetExtension GHC.ImplicitPrelude
  where
    setExtension = flip GHC.DynFlags.xopt_set
    unsetExtension = flip GHC.DynFlags.xopt_unset
