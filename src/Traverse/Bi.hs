module Traverse.Bi (module Traverse.Bi, module X) where
import Traverse.Bi.Internal
import Map.Bi as X
import FoldMap.Bi as X
import Applicative.Class as X

class (Bimap t,BifoldMap t) => Bitraverse t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse f g t = bisequence (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence :: Applicative f => t (f a) (f b) -> f (t a b)
  bisequence = bitraverse (\fa -> fa) (\fb -> fb)

bitraverse_bifoldMap :: (TimesOne m, Bitraverse t) => (a -> m) -> (b -> m) -> t a b -> m
bitraverse_bifoldMap f g t = thesek'biextract (bitraverse (\x -> ThisK (f x)) (\y -> ThatK (g y)) t)

class (BifoldMap0 t, Bitraverse t) => Bitraverse0 t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse0 :: Pure f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse0 f g t = bisequence0 (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence0 :: Pure f => t (f a) (f b) -> f (t a b)
  bisequence0 = bitraverse0 (\fa -> fa) (\fb -> fb)

class (BifoldMap1 t, Bitraverse t) => Bitraverse1 t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse1 :: Apply f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse1 f g t = bisequence1 (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence1 :: Apply f => t (f a) (f b) -> f (t a b)
  bisequence1 = bitraverse1 (\fa -> fa) (\fb -> fb)

{-bitraverse1_bifoldMap1 :: (Plus m, Bitraverse1 t) => (a -> m) -> (b -> m) -> t a b -> m-}
{-bitraverse1_bifoldMap1 f g t = thesek'biextract (bitraverse1 (\x -> ThisK (f x)) (\y -> ThatK (g y)) t)-}

class (BifoldMap_ t, Bitraverse0 t, Bitraverse1 t) => Bitraverse_ t where
--  {-# minimal traverse | cocollect | sequence #-}
  bitraverse_ :: Map f => (a -> f a') -> (b -> f b') -> t a b -> f (t a' b')
  bitraverse_ f g t = bisequence_ (bimap f g t)
  {-cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  bisequence_ :: Map f => t (f a) (f b) -> f (t a b)
  bisequence_ = bitraverse_ (\fa -> fa) (\fb -> fb)

instance Bitraverse1 (,) where bitraverse1 f g (a,b) = (,) `map` f a `ap` g b
instance Bitraverse (,) where bitraverse f g (a,b) = (,) `map` f a `ap` g b
