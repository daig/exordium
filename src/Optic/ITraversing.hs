module Optic.ITraversing where
{-import Star-}
import Arrow.Traversed as X
import Indexable.Class as X

{-newtype ITraversing i f a b = ITraversing {runITraversing :: i -> a -> f b}-}

{-{-instance Bind m => Compose (Star m) where Star f > Star g = Star (g <=< f)-}-}
{-{-instance Monad m => Category (Star m) where id = Star pure-}-}
{-instance Map f => Promap (ITraversing i f) where promap f g (ITraversing is) = ITraversing (\i -> promap f (map g) (is i))-}
{-instance Map f => Map (ITraversing i f a) where map f (ITraversing is) = ITraversing (\i a -> map f (is i a))-}

{-instance Indexed i (ITraversing i f) where-}
  {-type Unindexed (ITraversing i f) = Star f-}
  {-indexed (ITraversing is) i = Star (is i)-}
{-instance Applicative f => Traversed (ITraversing i f) where traversal afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-instance Pure f => Traversed0 (ITraversing i f) where traversal0 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-instance Apply f => Traversed1 (ITraversing i f) where traversal1 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-instance Map f => Traversed_ (ITraversing i f) where traversal_ afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-instance Pure f => Traversed' (ITraversing i f) where-}
  {-prism pat constr (ITraversing iafb) = ITraversing (\i s -> case pat s of-}
    {-L t -> pure t-}
    {-R a -> constr `map` iafb i a)-}
