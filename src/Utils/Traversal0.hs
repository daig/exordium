module Utils.Traversal0 (module Utils.Traversal0, module X) where
import Class.Traversal0 as X
import Utils.E

traversed0_left :: Traversal0 p => p a b -> p (E a y) (E b y)
traversed0_left = \p -> dimap e'swap e'swap (traversed0 p)
traversed0_right :: Traversal0 p => p a b -> p (E x a) (E x b)
traversed0_right = traversed0
