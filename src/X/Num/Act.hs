module X.Num.Act (Act(..), module X) where
import X.Num.Add as X

-- | A semigroup action: (m+n)s = m(ns)
--  Acts like scalar offset
class Add a => Act a s where act :: a -> s -> s

instance Act (a -> a) a where act f = f
