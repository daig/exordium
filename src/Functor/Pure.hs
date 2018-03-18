module Functor.Pure (Pure(..), module X) where
import Functor.Map as X
import Num.Zero
import Num.One as X
import {-# source #-} ADT.E as X
import {-# source #-} Type.K
import {-# source #-} Type.I

-- http://r6research.livejournal.com/28338.html
-- a Pure f is strong with respect to E

-- | Natural laws:
-- distR < right (map f) = map (right f) < distR
-- distR < left f = map (left f) < distR
-- 
-- Derived Laws:
-- distR < L = pure < L
-- dirtR < R = map R
-- 
class Map f => Pure f where pure :: a -> f a

instance Pure ((->) x) where pure a = \_ -> a
instance Pure [] where pure a = [a]
instance (Zero a) => Pure (K a) where pure _ = K zero
instance Pure I where pure = I
instance Pure (E x) where pure = R
instance Zero x => Pure ((,) x) where pure a = (zero,a)
