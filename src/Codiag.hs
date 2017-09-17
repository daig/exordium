module Codiag where
import Bimap
import Bool

class Bimap f => Codiag f where
  codiag :: f a a -> a
bimapCodiag :: (Codiag f, Eq a) => (x -> a) -> f x x -> Bool
bimapCodiag f faa = codiag (bimap f f faa) == f (codiag faa)
