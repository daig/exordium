module Arrow.Precoerce (Precoerce(..), precoerce__R, module X) where
import Arrow.Traversed as X

class Traversed' p => Precoerce p where
  {-# minimal from | precoerce #-}
  from :: (b -> t) -> p a b -> p s t
  from bt p = precoerce (postmap bt p)
  precoerce :: p a x -> p s x
  precoerce = from (\x -> x)

precoerce__R :: Precoerce p => p a b -> p (E x a) (E x b)
precoerce__R = from R
