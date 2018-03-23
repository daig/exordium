{-# language MagicHash #-}
module Functor.Unfold (module Functor.Unfold, module X) where
import Num.Pred as X
import {-# source #-} Type.K
import {-# source #-} Type.I
import {-# source #-} ADT.E
import {-# source #-} ADT.Maybe
import ADT.X as X
import Functor.Pure as X
import Functor.Empty as X
import Cast.Coerce.Unsafe

class Absurd a where absurd :: a -> b

class Unfold0 t where unfold0 :: t a -- s ~ X. same as Empty
class Unfold_ t where unfold_ :: a -> t a -- s ~ (). same as Pure

class Unfold t where
  unfoldmap :: Pred s => (s -> a) -> s -> t a
  {-unfoldmap f = unfoldr go where-}
    {-go s = case pred s of-}
      {-Nothing -> L empty-}
      {-Just s' -> R s'-}

  {-unfoldWith a next s0 = unfoldr (\s -> case next s of {L t -> t-}
  {-foldMap f t = foldr (\a m -> f a `add` m) zero t -- TODO: check the order-}
  unfoldr :: (m -> E (t a) m) -> m -> t a
  {-unfoldr f = unfoldmap go where-}
    {-go m = case f m of-}
      {-L t -> -}
  unfold :: Pred s => s -> t s

newtype P x m = P (m -> E x m)
instance Pred (P x m) where
  pred (P f) = P (\m -> case f m of
    L _ -> Nothing
    R m' -> case f m' of
      L _ -> Nothing
      R m'' -> Just m'')

  {-foldr c z t = foldMap c t z-} -- TODO: need an Add instances for (->)
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}

{-class Unfold t => Unfold0 t where-}
  {-foldMap0 :: Zero m => (a -> m) -> t a -> m-}
  {-{-foldMap0 = fold0 zero-}-}
  {-{-fold0 :: m -> (a -> m) -> t a -> m-}-}
  {-{-fold0 = foldMap0 zero-} -- TODO: use reflection-}

{--- | like @Unfold0@ but can use the context if there is no @a@.-}
{----}
{---   like @BifoldMap_@ with an implicit first component-}
{----}
{---   law: foldMap' coerce# pure = id.-}
{---   law: foldMap' coerce# (pure . f) = map f.-}
{---   The above only makes sense when @t@ is representational/parametric.-}

{-class Unfold t => Unfold1 t where-}
  {-foldMap1 :: Add s => (a -> s) -> t a -> s-}
  {-fold1 :: (a -> a -> a) -> t a -> a-}

{-class (Unfold0 t, Unfold1 t) =>  Unfold_ t where-}
  {-{-# minimal foldMap_ | fold_ #-}-}
  {-foldMap_ :: (a -> b) -> t a -> b-}
  {-foldMap_ f x = f (fold_ x)-}
  {-fold_ :: t a -> a-}
  {-fold_ = foldMap_ (\x -> x)-}
