module TraverseT (TraverseT(..), module X) where
import MapT as X
import Applicative as X

class MapT t => TraverseT t where
  traverseT :: Applicative m => (forall x. f x -> m (g x)) -> t f a -> m (t g a)
