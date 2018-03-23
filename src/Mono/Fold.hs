module Mono.Fold (MonoFold(..), module X) where
import Num.Add0 as X
import Arrow.Folded

class MonoFold s a | s -> a where
  _foldMap :: Folded p => p a b -> p s t
  {-default _foldMap :: (s ~ f a, FoldMap f) => Add0 m => (a -> m) -> s -> m-}
  {-_foldMap = foldMap-}

class MonoFold s a => MonoFold0 s a | s -> a where _foldMap0 :: Zero m => (a -> m) -> s -> m
class MonoFold s a => MonoFold1 s a | s -> a where _foldMap1 :: Add m => (a -> m) -> s -> m
class (MonoFold0 s a,MonoFold1 s a) => MonoFold_ s a | s -> a where _fold_ :: s -> a
