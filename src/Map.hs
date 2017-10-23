{-# language UndecidableInstances #-}
module Map where
import Prelude (($)) -- TOOD: reexport
import qualified Prelude as P

class Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  (!@) :: b -> f a -> f b
  (!@) b = map (\_ -> b)

($@) :: Map f => (a -> b) -> f a -> f b
($@) = map
(@$) :: Map f => f (a -> b) -> a -> f b
fab @$ a = ($ a) $@ fab
infixl 1 @$
f ## b = \a -> f a b

instance Map ((,) x) where map f (x,a) = (x,f a)
instance Map ((->) x) where map f g = \x -> f (g x)
instance Map [] where
  map f = go where
    go = \case
      [] -> []
      a:as -> f a : go as

instance Map f => P.Functor f where
  fmap = map
  (<$) = (!@)
