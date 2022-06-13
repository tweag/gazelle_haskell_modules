module PackageD.B where
    import {-# SOURCE #-} PackageD.A( TA(..) )

    data TB = MkTB !Int

    g :: TA -> TB
    g (MkTA x) = MkTB x