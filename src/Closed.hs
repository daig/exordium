module Closed (Closed(..), ($.), module X) where
import Dimap as X

class Dimap p => Closed p where closed :: p a b -> p (x -> a) (x -> b)

($.) :: Closed p => p (a,b) c -> p a (b -> c)
($.) = \p -> (,) `premap` closed p
