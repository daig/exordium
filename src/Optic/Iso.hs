module Optic.Iso (module Optic.Iso, module X) where
import Map.Pro
import Zero
import Optic.Prism as X

_1 :: (Zero z, Promap p) => p x a -> p (x,z) (a,z)
_1 = promap (\(a,_) -> a) (\a -> (a,zero))

