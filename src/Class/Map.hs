module Class.Map (module Class.Map, module X) where
import Class.MapIso as X
import Type.O
import Utils.K
import Utils.List
import Utils.Where
import Utils.E
import Utils.Tuple
import Utils.I
import Type.Option

class MapIso f => Map f where
  {-# minimal map #-}
  map :: (a -> b) -> f a -> f b
  constMap :: b -> f a -> f b
  constMap b = map (\_ -> b)


instance Map ((->) x) where map f p = \a -> f (p a)
instance Map (K a) where map _ = k'absurd
instance Map [] where map = list'map
instance Map (Where a) where map = where'map
instance (Map f,Map g) => Map (O f g) where map f (O fg) = O (map (map f) fg)
instance Map (E a) where map = e'map
instance Map ((,) x) where map = tuple'map
instance Map (?) where map f = \case {None -> None; Some a -> Some (f a)}
instance Map I where map = i'map
