module PackageC.B where
    import {-# SOURCE #-} PackageC.A( TA(..) )

    data TB = MkTB !Int

    g :: TA -> TB
    g (MkTA x) = MkTB x