module Utils.Closed (module Utils.Closed, module X) where
import Class.Closed as X

-- | curry
($.) :: Closed p => p (a,b) c -> p a (b -> c)
($.) = \p -> (,) `colmap` closed p
