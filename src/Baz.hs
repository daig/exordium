module Baz (module Baz, module X) where
import Utils.Bazaar as X
import Class.Traverse_ as X
import Utils.Traverse_
import Type.I
import Type.O
{-import Class.Traverse_-}


newtype Baz c t b a = Baz {runBaz :: forall f. c f => (a -> f b) -> f t}

sold' :: forall c a t. c I => (forall f. c f => (a -> f a) -> f t) -> t
sold' afaft = case afaft I of I t -> t
sold :: forall c t a. c I => Baz c t a a -> t
sold (Baz afaft) = sold' @c @a afaft

instance Traverse (Baz Map t b) where traverse = traverse_
instance Traverse0 (Baz Map t b) where traverse0 = traverse_
instance Traverse1 (Baz Map t b) where traverse1 = traverse_
instance Traverse_ (Baz Map t b) where
  traverse_ f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance FoldMap  (Baz Map t b) where foldMap  = traverse__foldMap_
instance FoldMap0 (Baz Map t b) where foldMap0 = traverse__foldMap_
instance FoldMap1 (Baz Map t b) where foldMap1 = traverse__foldMap_
instance FoldMap_ (Baz Map t b) where foldMap_ = traverse__foldMap_
instance Map (Baz Map t b) where map = traverse_map
instance MapIso (Baz Map t b) where mapIso _ = traverse_map

instance Traverse (Baz Pure t b) where traverse = traverse0
instance Traverse0 (Baz Pure t b) where
  traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))
instance FoldMap  (Baz Pure t b) where foldMap  = traverse0_foldMap0
instance FoldMap0 (Baz Pure t b) where foldMap0 = traverse0_foldMap0
instance Map (Baz Pure t b) where map = traverse_map
instance MapIso (Baz Pure t b) where mapIso _ = traverse_map

instance Traverse (Baz Apply t b) where traverse = traverse1
instance Traverse1 (Baz Apply t b) where
  traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))
instance FoldMap  (Baz Apply t b) where foldMap  = traverse1_foldMap1
instance FoldMap1 (Baz Apply t b) where foldMap1 = traverse1_foldMap1
instance Map (Baz Apply t b) where map = traverse_map
instance MapIso (Baz Apply t b) where mapIso _ = traverse_map

{-traverse_foldMap :: (forall x y. (x -> K m y) -> t x -> K m (t y)) -> (a -> m) -> t a -> m-}
{-traverse_foldMap traverse = \am ta -> case traverse (\a -> K (am a)) ta of K m' -> m'-}
{-baz'traverse :: forall c f a a' t b. (forall g x y. c g => (x -> y) -> g x -> g y)-}
             {--> (a -> f a') -> Baz c t b a -> f (Baz c t b a')-}
{-baz'traverse afa' (Baz afbft) =-}
  {-let-}
    {-q a = sell @c `map` afa' a-}
    {-afbft-}
{-baz'traverse map map' f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map' (sell @c) (f x)))))-}

{-baz'map :: (x -> a) -> Baz c t b x -> Baz c t b a-}
{-baz'map f (Baz t) = Baz (\afb -> t (\x -> afb (f x)))-}
{-baz'foldMap :: (forall x y. (x -> K m y) -> Baz c t b x -> K m y) -> (a -> m) -> Baz c t b a -> m-}
{-baz'foldMap traverse = \f t -> case traverse (\x -> K (f x)) t of K m -> m-}
