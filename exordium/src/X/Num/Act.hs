{-# language MagicHash #-}
module X.Num.Act (Act(..), module X) where
import X.Num.Add as X
import X.Data.Addr
import X.Prim.Addr
import X.Type.Int.I

-- | A semigroup action: (m+n)s = m(ns)
--  Acts like scalar offset
class Add a => Act a s where act :: a -> s -> s

instance Act (a -> a) a where act f = f

instance Act Int Addr where act (I# i) (Addr# a) = Addr# (plusAddr# a i)
