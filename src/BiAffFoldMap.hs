module BiAffFoldMap (BiAffFoldMap(..),module X) where
import BiFoldMap as X

class BiFoldMap t => BiAffFoldMap t where
  {-# minimal bifoldMap0 #-}
  bifoldMap0 :: Def m => (a -> m) -> (b -> m) -> t a b -> m
  foldMapL0 :: Def m => (a -> m) -> t a b -> m
  foldMapL0 f = bifoldMap0 f (\_ -> def)
  foldMapR0 :: Def m => (b -> m) -> t a b -> m
  foldMapR0 g = bifoldMap0 (\_ -> def) g
