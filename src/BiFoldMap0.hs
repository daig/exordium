module BiFoldMap0 (BiFoldMap0(..),module X) where
import BiFoldMap as X

class BiFoldMap t => BiFoldMap0 t where
  {-# minimal bifoldMap0 #-}
  bifoldMap0 :: Zero m => (a -> m) -> (b -> m) -> t a b -> m
  foldMapL0 :: Zero m => (a -> m) -> t a b -> m
  foldMapL0 f = bifoldMap0 f (\_ -> zero)
  foldMapR0 :: Zero m => (b -> m) -> t a b -> m
  foldMapR0 g = bifoldMap0 (\_ -> zero) g
