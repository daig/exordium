module IStar where
{-import Star-}
import Traversed as X
import Indexable.Class as X

{-newtype IStar i f a b = IStar {runIStar :: i -> a -> f b}-}

{-{-instance Bind m => Compose (Star m) where Star f > Star g = Star (g <=< f)-}-}
{-{-instance Monad m => Category (Star m) where id = Star pure-}-}
{-instance Map f => Promap (IStar i f) where promap f g (IStar is) = IStar (\i -> promap f (map g) (is i))-}
{-instance Map f => Map (IStar i f a) where map f (IStar is) = IStar (\i a -> map f (is i a))-}

{-instance Indexed i (IStar i f) where-}
  {-type Unindexed (IStar i f) = Star f-}
  {-indexed (IStar is) i = Star (is i)-}
{-instance Applicative f => Traversed (IStar i f) where traversal afbsft (IStar iafb) = IStar (\i s -> afbsft (iafb i) s)-}
{-instance Pure f => Traversed0 (IStar i f) where traversal0 afbsft (IStar iafb) = IStar (\i s -> afbsft (iafb i) s)-}
{-instance Apply f => Traversed1 (IStar i f) where traversal1 afbsft (IStar iafb) = IStar (\i s -> afbsft (iafb i) s)-}
{-instance Map f => Traversed_ (IStar i f) where traversal_ afbsft (IStar iafb) = IStar (\i s -> afbsft (iafb i) s)-}
{-instance Pure f => Traversed' (IStar i f) where-}
  {-prism pat constr (IStar iafb) = IStar (\i s -> case pat s of-}
    {-L t -> pure t-}
    {-R a -> constr `map` iafb i a)-}
