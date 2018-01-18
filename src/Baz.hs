module Baz (Baz(..),sold,module X) where
import Bazaar
import Traverse_ as X
import I
import K
import O


foldMap_Zeroault :: (Traverse_ t) => (a -> m) -> t a -> m
foldMap_Zeroault f t = case traverse_ (\x -> K (f x)) t of {K m -> m}
foldMapZeroault :: (Traverse t, PlusZero m) => (a -> m) -> t a -> m
foldMapZeroault f t = case traverse (\x -> K (f x)) t of {K m -> m}
foldMap1Zeroault :: (Traverse1 t, Plus m) => (a -> m) -> t a -> m
foldMap1Zeroault f t = case traverse1 (\x -> K (f x)) t of {K m -> m}
foldMap0Zeroault :: (Traverse0 t, Zero m) => (a -> m) -> t a -> m
foldMap0Zeroault f t = case traverse0 (\x -> K (f x)) t of {K m -> m}

newtype Baz c t b a = Baz {runBaz :: forall f. c f => (a -> f b) -> f t}

sold :: c I => Baz c t a a -> t
sold m = case runBaz m I of {I t -> t}

instance MapIso (Baz c t b) where mapIso = map_mapIso
instance Map (Baz c t b) where map f (Baz t) = Baz (\afb -> t (\x -> afb (f x)))

instance FoldMap (Baz Map t b) where foldMap = foldMapZeroault
instance Traverse (Baz Map t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance FoldMap0 (Baz Map t b) where foldMap0 = foldMap0Zeroault
instance Traverse0 (Baz Map t b) where
  traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance FoldMap1 (Baz Map t b) where foldMap1 = foldMap1Zeroault
instance Traverse1 (Baz Map t b) where
  traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance FoldMap_ (Baz Map t b) where
 foldMap_ = foldMap_Zeroault 

instance Traverse_ (Baz Map t b) where
  traverse_ f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))

instance FoldMap (Baz Pure t b) where foldMap = foldMapZeroault
instance Traverse (Baz Pure t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))
instance FoldMap0 (Baz Pure t b) where foldMap0 = foldMap0Zeroault
instance Traverse0 (Baz Pure t b) where
  traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))

instance FoldMap (Baz Apply t b) where foldMap = foldMapZeroault
instance Traverse (Baz Apply t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))
instance FoldMap1 (Baz Apply t b) where foldMap1 = foldMap1Zeroault
instance Traverse1 (Baz Apply t b) where
  traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))

instance FoldMap (Baz Applicative t b) where foldMap = foldMapZeroault
instance Traverse (Baz Applicative t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Applicative) (f x)))))
