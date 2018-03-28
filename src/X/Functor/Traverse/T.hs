module X.Functor.Traverse.T (TraverseT(..), module X) where
import X.Transformer.Map as X
import X.Functor.Applicative as X

class TMap t => TraverseT t where
  traverseT :: Applicative m => (forall x. f x -> m (g x)) -> t f a -> m (t g a)
