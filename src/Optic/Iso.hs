module Optic.Iso where
import Map.Di
import Zero
import Optic.Prism

_1 :: (Zero z, Dimap p) => p x a -> p (x,z) (a,z)
_1 = dimap (\(a,_) -> a) (\a -> (a,zero))
