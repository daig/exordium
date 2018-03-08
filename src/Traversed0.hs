module Traversed0 (module Traversed0, module X) where
import Traversed0.Class as X
import Dimap
import E.Utils

traversed0_left :: Traversed0 p => p a b -> p (E a y) (E b y)
traversed0_left = \p -> dimap e'swap e'swap (traversed0 p)
traversed0_right :: Traversed0 p => p a b -> p (E x a) (E x b)
traversed0_right = traversed0
