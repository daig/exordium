module Class.TraverseT (module Class.TraverseT, module X) where
import Class.MapT as X
import Class.Applicative as X

class MapT t => TraverseT t where
  traverseT :: Applicative m => (forall x. f x -> m (g x)) -> t f a -> m (t g a)
