module Functor.ITraverse (module Functor.ITraverse , module X) where
import Functor.Fold as X
import Functor.Applicative as X
import Functor.Comonad as X
import {-# source #-} Type.K
import Type.I
import ADT.Maybe
import Functor.Traverse as X
import Num.FromNatural

class (Map t,Fold t) => ITraverse i t where
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

itraverse_imap :: ITraverse i t => (i -> a -> b) -> t a -> t b
itraverse_imap f ta = case itraverse (\i a -> I (f i a)) ta of I tb -> tb

itraverse_ifoldMap :: (ITraverse i t,Add0 m) => (i -> a -> m) -> t a -> m
itraverse_ifoldMap f ta = case itraverse (\i a -> K (f i a)) ta of K m -> m

class (ITraverse i t,Fold0 t) => ITraverse0 i t where
  itraverse0 :: Pure f => (i -> a -> f b) -> t a -> f (t b)

itraverse0_ifoldMap0 :: (ITraverse0 i t,Zero m) => (i -> a -> m) -> t a -> m
itraverse0_ifoldMap0 f ta = case itraverse0 (\i a -> K (f i a)) ta of K m -> m

class (ITraverse i t,Fold1 t) => ITraverse1 i t where
  itraverse1 :: Apply f => (i -> a -> f b) -> t a -> f (t b)


itraverse1_ifoldMap1 :: (ITraverse1 i t,Add m) => (i -> a -> m) -> t a -> m
itraverse1_ifoldMap1 f ta = case itraverse1 (\i a -> K (f i a)) ta of K m -> m

class (ITraverse0 i t, ITraverse1 i t,Fold_ t, Comonad t) => ITraverse_ i t where
  itraverse_ :: Map f => (i -> a -> f b) -> t a -> f (t b)

itraverse__ifoldMap_ :: ITraverse_ i t => (i -> a -> m) -> t a -> m
itraverse__ifoldMap_ f ta = case itraverse_ (\i a -> K (f i a)) ta of K m -> m

instance Zero i => ITraverse i I where itraverse = itraverse_
instance Zero i => ITraverse0 i I where itraverse0 = itraverse_
instance Zero i => ITraverse1 i I where itraverse1 = itraverse_
instance Zero i => ITraverse_ i I where itraverse_ iafb (I a) = I `map` iafb zero a

instance Zero i => ITraverse0 i ((,) x) where itraverse0 f (x,a) = (x,) `map` f zero a
instance Zero i => ITraverse1 i ((,) x) where itraverse1 f (x,a) = (x,) `map` f zero a
instance Zero i => ITraverse_ i ((,) x) where itraverse_ f (x,a) = (x,) `map` f zero a
instance Zero i => ITraverse i ((,) x) where itraverse f (x,a) = (x,) `map` f zero a
instance FromNatural i => ITraverse i [] where
  itraverse = go' where
    go' f = go zero where
      go i = \case
        [] -> pure []
        (x:xs) -> (:) `map` f i x  `ap` go (add one i) xs
{-instance ITraverse i (K x) where itraverse f (K x) = pure (K x)-}
{-instance ITraverse i0 (K x) where itraverse0 f (K x) = pure (K x)-}

{-instance ITraverse i (E x) where itraverse = itraverse0-}
{-instance ITraverse i0 (E x) where-}
  {-itraverse0 f = \case-}
    {-L x -> pure (L x)-}
    {-R a -> R `map` f a-}


{-instance ITraverse i0 Maybe where-}
  {-itraverse0 afb = \case-}
    {-Nothing -> pure Nothing-}
    {-Just a -> Just `map` afb a-}
{-instance ITraverse i Maybe where itraverse = itraverse0-}
{-instance ITraverse i' Maybe where-}
  {-itraverse' f0 f = \case {Nothing -> map (\case) (f0 Nothing); Just a -> Just `map` f a}-}

{-instance ITraverse i' I where-}
  {-itraverse' _ f (I a) = I `map` f a-}
