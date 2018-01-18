module BiFoldMap (BiFoldMap(..), module X) where
import PlusZero as X

class BiFoldMap t where
  {-# minimal bifoldMap | foldMapL, foldMapR #-}
  bifoldMap :: PlusZero m => (a -> m) -> (b -> m) -> t a b -> m
  foldMapL :: PlusZero m => (a -> m) -> t a b -> m
  foldMapR :: PlusZero m => (b -> m) -> t a b -> m
  {-foldr :: (a -> b -> b) -> b -> t a -> b-}
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}
