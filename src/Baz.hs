module Baz (Baz(..),sold,module X) where
import Bazaar
import Traverse_ as X
import I
import O


newtype Baz c t b a = Baz {runBaz :: forall f. c f => (a -> f b) -> f t}

sold :: c I => Baz c t a a -> t
sold m = case runBaz m I of {I t -> t}

instance Map (Baz c t b) where map f (Baz t) = Baz (\afb -> t (\x -> afb (f x)))

instance FoldMap (Baz Map t b) where foldMap = foldMapDefault
instance Traverse (Baz Map t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance FoldMap0 (Baz Map t b) where foldMap0 = foldMap0Default
instance Traverse0 (Baz Map t b) where
  traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance FoldMap1 (Baz Map t b) where foldMap1 = foldMap1Default
instance Traverse1 (Baz Map t b) where
  traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance FoldMap_ (Baz Map t b) where foldMap_ = foldMap_Default
instance Traverse_ (Baz Map t b) where
  traverse_ f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))

instance FoldMap (Baz Pure t b) where foldMap = foldMapDefault
instance Traverse (Baz Pure t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))
instance FoldMap0 (Baz Pure t b) where foldMap0 = foldMap0Default
instance Traverse0 (Baz Pure t b) where
  traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))

instance FoldMap (Baz Apply t b) where foldMap = foldMapDefault
instance Traverse (Baz Apply t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))
instance FoldMap1 (Baz Apply t b) where foldMap1 = foldMap1Default
instance Traverse1 (Baz Apply t b) where
  traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))

instance FoldMap (Baz Applicative t b) where foldMap = foldMapDefault
instance Traverse (Baz Applicative t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Applicative) (f x)))))
