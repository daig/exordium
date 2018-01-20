{-# language MagicHash #-}
module Map.Impl where
import Flip
import Isos.Re
import Forall
import Void
import Coerce
import K.Type
import These.Type
import Where.Type
import Flip

  {-default dimap :: Map (p x) => (a -> x) -> (y -> b) -> p x y -> p a b-}
  {-dimap f g p = colmap f (map g p)-}

class CoRMap p where
  cormap :: (b -> x) -> p a x -> p a b
  {-default cormap :: Comap (p a) => (b -> x) -> p a x -> p a b-}
  {-cormap = comap-}



{- DEFAULTS -}

{- INSTANCES -}

{-instance Bimap p => Bimap (Flipped p) where bimap f g (Flip p) = Flip (bimap g f p)-}
{-instance LMap p => RMap (Flipped p b) where map f (Flip x) = Flip (lmap f x)-}
{-instance LMap p => Map (Flipped p b)  where map = rmap_map-}
{-instance CoLMap p => CoRMap (Flipped p b) where cormap f (Flip x) = Flip (colmap f x)-}
{-instance CoRMap p => Comap (Flipped p b) where comap = cormap_comap-}
{-instance MapIso p => MapIso (Flipped p b) where mapIso = map_mapIso-}

instance Map ((,) x) where map = bimap_map
instance MapIso ((,) x) where mapIso = map_mapIso
{-instance Map (Flipped (,) b) where map = bimap_map-}
{-instance MapIso (Flipped (,) x) where mapIso = map_mapIso-}




instance Dimap (->) where dimap f g h = \x -> g (h (f x))
instance CoLMap (->)
instance Comap (Flipped (->) b) where comap = dimap_comap
instance Map ((->) x) where map = dimap_map
instance MapIso ((->) x) where mapIso = map_mapIso

instance Bimap K where bimap f _ (K a) = K (f a)
instance RMap K where rmap = bimap_rmap
instance Map (K a) where map = rmap_map
instance LMap K where lmap = bimap_lmap
instance MapIso (K a) where mapIso = map_mapIso
{-instance Map (Flipped K b) where map = bimap_map-}
{-instance MapIso (Flipped K b) where mapIso = map_mapIso-}

instance Dimap (Flipped K)
instance Comap (K a) where comap _ = coerce#

{-instance Dimap p => Dimap (Re p s t) where dimap f g (Re r) = Re (\x -> r (dimap g f x))-}

instance Bimap These where
  bimap f g = \case
    This a -> This (f a)
    That b -> That (g b)
    These a b -> These (f a) (g b)
instance RMap These where rmap = bimap_rmap
instance LMap These where lmap = bimap_lmap
instance Map (These a) where map = rmap_map
{-instance Map (Flipped These b) where map = bimap_lmap-}
instance MapIso (These a) where mapIso = map_mapIso
{-instance MapIso (Flipped These b) where mapIso = map_mapIso-}

instance Bimap Where where bimap = where'bimap
instance Map (Where a) where map = bimap_map
instance RMap Where where rmap = bimap_rmap
instance LMap Where where lmap = bimap_lmap
{-instance Map (Flipped Where b) where map = bimap_lmap-}
instance MapIso (Where a) where mapIso = map_mapIso
{-instance MapIso (Flipped Where b) where mapIso = map_mapIso-}
