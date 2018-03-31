{-# language UnboxedTuples #-}
module X.Functor.Comonad (Comonad, module X) where
import X.Functor.Fold as X
import X.Functor.Duplicate as X
import X.Type.I


-- | extend extract = id
-- extract < extend f = f
class (Fold_ w,Duplicate w) => Comonad w

instance Comonad ((,) x)
instance Comonad I
