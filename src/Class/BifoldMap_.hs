module Class.BifoldMap_ where

class BifoldMap_ p where bifoldMap_ :: (x -> a) -> (y -> a) -> p x y -> a
