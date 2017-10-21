module ApplyP where
import Dimap


class Dimap p => ApplyP p where (<*>) :: p a b -> p x y -> p (a,x) (b,y)

instance ApplyP (->) where f <*> g = \(a,x) -> (f a, g x)
