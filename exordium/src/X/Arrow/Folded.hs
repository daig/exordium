module X.Arrow.Folded (module X.Arrow.Folded, module X) where
import X.Arrow.Folded.Internal
import X.Arrow.Traversed as X

class Traversed_ p => Folded_ p where
  {-# minimal folding_ | postcoerce #-}
  folding_ :: (s -> a) -> p a b -> p s t
  folding_ sa p = postcoerce (premap sa p)
  folded_ :: (Fold_ f) => p a b -> p (f a) t
  folded_ = folding_ fold_
  postcoerce :: p x b -> p x t
  postcoerce p = premap (pure @(FreeFold Trivial)) (folded_ p)

class (Folded_ p, Traversed1 p) => Folded1 p where
  {-# minimal folding1, folded1 #-}
  folding1 :: (forall m. Add m => (a -> m) -> s -> m) -> p a b -> p s t
  folding1 amsm p = premap (amsm (pure @(FreeFold Add))) (folded1 p)
  folded1 :: Fold1 f => p a b -> p (f a) t
  folded1 = folding1 (\am s -> foldMap1 am s)


class (Folded_ p, Traversed0 p) => Folded0 p where
  folding0 :: (forall m. Zero m => (a -> m) -> s -> m) -> p a b -> p s t
  folding0 amsm p = premap (amsm (pure @(FreeFold Zero))) (folded0 p)
  folded0 :: Fold0 f => p a b -> p (f a) t
  folded0 = folding0 (\am s -> foldMap0 am s)

class (Folded0 p, Folded1 p, Traversed p) => Folded p where
  folding :: (forall m. Add0 m => (a -> m) -> s -> m) -> p a b -> p s t
  folding amsm p = premap (amsm (pure @(FreeFold Add0))) (folded p)
  folded :: Fold f => p a b -> p (f a) t
  folded = folding (\am s -> foldMap am s)

postcoerce__2 :: Folded_ p => p a b -> p (x,a) (x,b)
postcoerce__2 = folding_ (\(_,a) -> a)
