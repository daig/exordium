module Num.Act (Act(..), module X) where
import Num.Add as X

-- | A semigroup action: (m+n)s = m(ns)
--  Acts like scalar offset
class Add a => Act a s where act :: a -> s -> s
