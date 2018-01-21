module Utils.Closed (module Utils.Closed, module X) where
import Closed.Class as X

-- | curry
($.) :: Closed p => p (a,b) c -> p a (b -> c)
($.) = \p -> (,) `colmap` closed p
