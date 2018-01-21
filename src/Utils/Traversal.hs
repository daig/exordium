module Utils.Traversal (module Utils.Traversal, module X) where
import Traversal.Class as X

traversed_dimap :: Traversal p => (a -> x) -> (y -> b) -> p x y -> p a b
traversed_dimap f g = traversal (\xfy a -> map g (xfy (f a)))
