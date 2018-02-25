module Pure.Class (module Pure.Class, module X) where
import Map.Class as X
import Lifts.Class as X
import Zero.Class
import One.Class as X
import {-# source #-} E as X
import E.Utils
import {-# source #-} K
import Any
import Bimap.Class

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
