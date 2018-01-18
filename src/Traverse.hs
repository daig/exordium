module Traverse (Traverse(..),module X) where
import Map as X
import Apply as X
import FoldMap as X
import Applicative as X
import Bind

class (Map t,FoldMap t) => Traverse t where
  {-# minimal traverse | cocollect | sequence #-}
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f t = cocollect (\x -> x) (map f t)
  cocollect :: Applicative f => (t a -> b) -> t (f a) -> f b
  cocollect tab tfa = map tab (sequence tfa)
  sequence :: Applicative f => t (f a) -> f (t a)
  sequence = traverse (\x -> x)

{-foldMapDefault :: (Traverse t, Zero m) => (a -> m) -> t a -> m-}
{-foldMapDefault f t = case traverse (\x -> K (f x)) t of {K m -> m}-}

instance Traverse ((,) x) where traverse f (x,a) = (x,) `map` f a
instance Traverse [] where
  traverse f = go where
    go = \case
      [] -> pure []


