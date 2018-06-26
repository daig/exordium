module X.Data.Pair where

-- | The same as a regular Haskell pair, but
--
-- @
-- (x :* _|_) = (_|_ :* y) = _|_
-- @
data StrictPair a b = a :* b

infixr 1 :*

toPair :: StrictPair a b -> (a, b)
{-# INLINE toPair #-}
-- | Convert a strict pair to a standard pair.
toPair (x :* y) = (x, y)
