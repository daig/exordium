module X.Num.MaxBottom (MaxBottom, module X) where
import X.Num.OrdMax' as X
import X.Num.Max as X
import X.Num.Bottom as X

-- | max bottom a = max' a top = Just a.
--
-- The above implies a total order. ie. @Max a@ is free
class (OrdMax' a, Max a, Bottom a) => MaxBottom a
