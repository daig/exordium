{-# language MagicHash #-}
module Fold (module Fold, module X) where
import Num.Add0 as X
import {-# source #-} Type.K
import {-# source #-} Type.I
import {-# source #-} E
import X
import Coerce
import Pure as X

class Fold t where
  {-# minimal foldMap | foldr #-}
  foldMap :: Add0 m => (a -> m) -> t a -> m
  foldMap f t = foldr (\a m -> f a `add` m) zero t -- TODO: check the order
  foldr :: (a -> b -> b) -> b -> t a -> b
  {-foldr c z t = foldMap c t z-} -- TODO: need an Add instances for (->)
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}

class Fold t => Fold0 t where
  foldMap0 :: Zero m => (a -> m) -> t a -> m
  {-foldMap0 = fold0 zero-}
  {-fold0 :: m -> (a -> m) -> t a -> m-}
  {-fold0 = foldMap0 zero-} -- TODO: use reflection

-- | like @Fold0@ but can use the context if there is no @a@.
--
--   like @BifoldMap_@ with an implicit first component
--
--   law: foldMap' coerce# pure = id.
--   law: foldMap' coerce# (pure . f) = map f.
--   The above only makes sense when @t@ is representational/parametric.
class (Fold0 t, Pure t) => Fold' t where
  foldMap' :: (forall x. t x -> b) -> (a -> b) -> t a -> b
  foldMap' l r ta = case fold' ta of
    L tx -> l tx
    R a -> r a
  fold' :: forall a x. t a -> E (t x) a
  fold' = foldMap' (\tx -> L (mapCoerce# tx)) R
instance Fold' (E x) where
  foldMap' txb ab = \case {l@L{} -> txb l; R a -> ab a}
instance Zero x => Fold' ((,) x) where
  foldMap' txb ab (x,a) = ab a

class Fold t => Fold1 t where
  foldMap1 :: Add s => (a -> s) -> t a -> s
  {-fold1 :: (a -> a -> a) -> t a -> a-}

class (Fold0 t, Fold1 t) =>  Fold_ t where
  {-# minimal foldMap_ | fold_ #-}
  foldMap_ :: (a -> b) -> t a -> b
  foldMap_ f x = f (fold_ x)
  fold_ :: t a -> a
  fold_ = foldMap_ (\x -> x)

  {-traverseL :: (a -> E a x) -> t a -> E (t a) x-}
  {-traverseL t = sequenceL < map t-}
  {-traverseR :: (a -> E x a) -> t a -> E x (t a)-}
  {-traverseR t = sequenceR < map t-}
  {-sequenceL :: t (E a b) -> E (t a) b -}
  {-sequenceL t = case fold_ t of {L _ -> L $ map (\(L a) -> a) t; R b -> R b}-}
  {-sequenceR :: t (E a b) -> E a (t b)-}
  {-sequenceR t = case fold_ t of {L b -> L b; R _ -> R $ map (\(R b) -> b) t}-}

  {-traverseFst :: (a -> (a,x)) -> t a -> (t a, x)-}
  {-traverseFst f = sequenceFst < map f-}
  {-sequenceFst :: t (a,b) -> (t a, b)-}
  {-sequenceFst t = ((\(a,_) -> a) $@ t, fold_ $ (\(_,b) -> b) $@ t)-}
  {-traverseSnd :: (a -> (x,a)) -> t a -> (x, t a)-}
  {-traverseSnd t = sequenceSnd < map t-}
  {-sequenceSnd :: t (a,b) -> (a,t b)-}
  {-sequenceSnd t = (fold_ $ (\(a,_) -> a) $@ t, (\(_,b) -> b) $@ t)-}


instance Fold0 (K x) where foldMap0 = \_ _ -> zero
instance Fold (K x) where foldMap = foldMap0

instance Fold_ ((,) x) where foldMap_ f (_,y) = f y
instance Fold0 ((,) x) where foldMap0 = foldMap_
instance Fold1 ((,) x) where foldMap1 = foldMap_
instance Fold ((,) x) where foldMap = foldMap_

instance Fold [] where foldMap = list'foldMap zero add
list'foldMap :: acc -> (x -> acc -> acc) -> (a -> x) -> [a] -> acc
list'foldMap zero add = go' where
  go' f = go where
    go = \case
      [] -> zero
      a:as -> f a `add` go as

instance Fold_ I where foldMap_ f (I a) = f a
instance Fold0 I where foldMap0 = foldMap_
instance Fold1 I where foldMap1 = foldMap_
instance Fold  I where foldMap = foldMap_

instance Fold (E x) where foldMap = foldMap0
instance Fold0 (E x) where
  foldMap0 f = \case
    L{} -> zero
    R a -> f a
