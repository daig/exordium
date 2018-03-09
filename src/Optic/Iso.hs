module Optic.Iso where
import Map.Pro
import Zero
import Optic.Prism

_1 :: (Zero z, Promap p) => p x a -> p (x,z) (a,z)
_1 = dimap (\(a,_) -> a) (\a -> (a,zero))
