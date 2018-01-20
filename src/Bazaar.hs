module Bazaar where
import Class.Applicative
import Class.Dimap as X
import Utils.Dimap

newtype Bazaar c a b t = Bazaar {runBazaar :: forall f. c f => (a -> f b) -> f t}

instance MapIso (Bazaar Map a b) where mapIso = map_mapIso
instance CoLMap (Bazaar Map a) where colmap = dimap_colmap
instance RMap (Bazaar Map a) where rmap = dimap_rmap
instance Map (Bazaar Map a b) where map = rmap_map
instance Dimap (Bazaar Map a) where dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))

instance Map (Bazaar Pure a b) where map = rmap_map
instance CoLMap (Bazaar Pure a) where colmap = dimap_colmap
instance RMap (Bazaar Pure a) where rmap = dimap_rmap
instance MapIso (Bazaar Pure a b) where mapIso = map_mapIso
instance Pure (Bazaar Pure a b) where pure a = Bazaar (\_ -> pure a)
instance Dimap (Bazaar Pure a) where dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))

instance Map (Bazaar Apply a b) where map = rmap_map
instance CoLMap (Bazaar Apply a) where colmap = dimap_colmap
instance RMap (Bazaar Apply a) where rmap = dimap_rmap
instance MapIso (Bazaar Apply a b) where mapIso = map_mapIso
instance Apply (Bazaar Apply a b) where (Bazaar mf) `ap` (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)
instance Dimap (Bazaar Apply a) where dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f  `map` k x))

instance Map (Bazaar Applicative a b) where map = rmap_map
instance MapIso (Bazaar Applicative a b) where mapIso = map_mapIso
instance Pure (Bazaar Applicative a b) where pure a = Bazaar (\_ -> pure a)
instance Apply (Bazaar Applicative a b) where (Bazaar mf) `ap` (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)
instance Applicative (Bazaar Applicative a b)
instance CoLMap (Bazaar Applicative a) where colmap = dimap_colmap
instance RMap (Bazaar Applicative a) where rmap = dimap_rmap
instance Dimap (Bazaar Applicative a) where dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))
