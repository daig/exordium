module TraverseT.Class (module TraverseT.Class, module X) where
import TMap.Class as X
import Applicative.Class as X

class TMap t => TraverseT t where
  traverseT :: Applicative m => (forall x. f x -> m (g x)) -> t f a -> m (t g a)
