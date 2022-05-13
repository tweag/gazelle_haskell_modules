module HImportScan.GHC.Utils
  ( toggleDynFlags
  ) where

import DynFlags (DynFlags(..))
import qualified DynFlags as GHC.DynFlags
import qualified GHC.LanguageExtensions as GHC

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
