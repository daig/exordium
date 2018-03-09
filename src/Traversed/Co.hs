module Traversed.Co (module Traversed.Co, module X) where
import Map.Di as X
import E

class Dimap p => CoTraversed' p where
  {-# minimal unleft | unright #-}
  unleft :: p (E a y) (E b y) -> p a b
  unleft p = unright (dimap swap swap p)
  unright :: p (E x a) (E x b) -> p a b
  unright p = unleft (dimap swap swap p)
