module Functor.Traverse.T (TraverseT(..), module X) where
import Transformer.Map as X
import Functor.Applicative as X

class TMap t => TraverseT t where
  traverseT :: Applicative m => (forall x. f x -> m (g x)) -> t f a -> m (t g a)
