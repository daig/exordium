module Strong (Strong(..), ($:), module X) where
import Dimap as X
import Swap as X

class Dimap p => Strong p where
  {-# minimal lens | first | second #-}
  lens :: (s -> a) -> (s -> b -> t) -> p a b -> p s t
  lens get set = \p -> dimap (\x -> (x,get x)) (\(s,b) -> set s b) (second p)
  first :: p a b -> p (a,y) (b,y)
  first = lens (\(a,_) -> a) (\(_,c) b -> (b,c))
  second :: p a b -> p (x,a) (x,b)
  second = \p -> dimap swap swap (first p)

($:) :: Strong p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `postmap` first p

instance Strong (->) where
  first f = \(a,y) -> (f a,y)
  second f = \(x,b) -> (x,f b)
