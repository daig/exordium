module X.Num.OrdMin' (OrdMin', module X) where
import X.Num.Ord' as X
import X.Num.Min' as X

-- | comparable a b => lte (min' a b) (Just a) && lte (min' a b) (Just b) = True
class (Min' a,Ord' a) => OrdMin' a
