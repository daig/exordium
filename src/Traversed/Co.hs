module Traversed.Co (module Traversed.Co, module X) where
import Map.Pro as X
import E

class Promap p => CoTraversed' p where
  {-# minimal unleft | unright #-}
  unleft :: p (E a y) (E b y) -> p a b
  unleft p = unright (promap swap swap p)
  unright :: p (E x a) (E x b) -> p a b
  unright p = unleft (promap swap swap p)
