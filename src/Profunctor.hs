{-# language MagicHash #-}
module Profunctor (Profunctor(..), module X) where
import Category as X

class (Category (Precat p), Category (Postcat p)) => Profunctor (p :: k -> k' -> *) where
  {-# minimal dimap' | premap',postmap' #-}
  type Precat p :: k -> k -> *
  type Postcat p :: k' -> k' -> *
  dimap' :: Precat p a x -> Postcat p y b -> p x y -> p a b
  dimap' f g = postmap' g < premap' f
  premap' :: Precat p a x -> p x b -> p a b
  premap' f = dimap' f id
  postmap' :: Postcat p y b -> p a y -> p a b
  postmap' = dimap' id


