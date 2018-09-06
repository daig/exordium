module X.Num.OrdMax' (OrdMax', module X) where
import X.Num.Ord' as X
import X.Num.Max' as X

-- | comparable a b => lte (max' a b) (Just a) && lte (max' a b) (Just b) = True
class (Max' a,Ord' a) => OrdMax' a
