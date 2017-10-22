module Close (Close(..), module X) where
import Map as X

class Map f => Close f where close :: (x -> f a) -> f (x -> a)
instance Close ((->) z) where close xza = \z x -> xza x z
