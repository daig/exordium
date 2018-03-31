module X.Num.MinTop (MinTop, module X) where
import X.Num.OrdMin' as X
import X.Num.Min as X
import X.Num.Top as X


-- | min' top a = min' a top = Just a.
--
-- The above implies a total order. ie. @Min a@ is free
class (OrdMin' a, Min a, Top a) => MinTop a
