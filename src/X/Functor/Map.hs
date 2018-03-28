{-# language MagicHash #-}
{-# language UnboxedTuples #-}
module X.Functor.Map (module X.Functor.Map, module X) where
import X.Cast.Coerce as X (type ( #=# ),coerce,coerceF)
import X.Cast.Coerce.Unsafe (coerceF#)
import {-# source #-} X.Type.K
import {-# source #-} X.Type.I
import {-# source #-} X.Data.E
import X.Data.Maybe
import X.Type.IO

class Map (f :: * -> *) where
  map :: (a -> b) -> f a -> f b
  -- | Try to coerce if @f@ is parametric.
  map# :: a #=# b => (a -> b) -> f a -> f b
  map# f !x = map f x
  -- | Inform the outer @g@ whether @f@ is parametric.
  -- This is an ugly hack to get correct behavior for @O f g@.
  -- TODO: see if there's a better way, or if its even necessary
  map## :: (Map g, a #=# b) => (a -> b) -> g (f a) -> g (f b)
  map## f !x = map (map# f) x
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)

para'map## :: (Map f, Map g, f a #=# f b, a #=# b) => (a -> b) -> g (f a) -> g (f b)
para'map## _ = map# coerce


map_mapIso :: Map f => (b -> a) -> (a -> b) -> f a -> f b
map_mapIso _ = map

mapAs :: forall g f a b. (Map g, f a #=# g a, g b #=# f b) => (a -> b) -> f a -> f b
mapAs f fa = coerceF @f (map f (coerceF @g fa))

mapAs# :: forall g f a b. Map g => (a -> b) -> f a -> f b
mapAs# f fa = coerceF# @f (map f (coerceF# @g fa))

instance Map ((->) x) where
  map f p = \a -> f (p a)
  map# _ = coerce
  map## _ = map# coerce
instance Map [] where
  map f = go where
    go = \case
      [] -> []
      a:as -> f a : go as
instance Map ((,) x) where map f (x,y) = (x,f y)
instance Map (K a) where map _ = coerce
instance Map I where map f (I a) = I (f a)

instance Map (E x) where
  map f = \case
    L a -> L a
    R b -> R (f b)

instance Map Maybe where map f = \case {Nothing -> Nothing; Just a -> Just (f a)}
maybe'map :: (a -> b) -> Maybe a -> Maybe b
maybe'map f = \case
  Nothing -> Nothing
  Just a -> Just (f a)

instance Map IO where map f (IO io) = IO (\s -> case io s of (# s', a #) -> (# s', f a #))
