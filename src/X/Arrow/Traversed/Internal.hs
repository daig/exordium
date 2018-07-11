module X.Arrow.Traversed.Internal where
import X.Functor.Traverse
import X.Arrow.Promap
import X.Type.I
import X.Arrow.Traversed.Internal.O

-- TODO: merge implementations if possible

newtype Baz c t b a = Baz {runBaz :: forall f. c f => (a -> f b) -> f t}

sold :: forall c t a. c I => Baz c t a a -> t
sold (Baz afaft) = case afaft I of I t -> t

instance Traverse (Baz Map t b) where traverse = traverse_
instance Traverse0 (Baz Map t b) where traverse0 = traverse_
instance Traverse1 (Baz Map t b) where traverse1 = traverse_
instance Traverse_ (Baz Map t b) where
  traverse_ f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))
instance Comonad (Baz Map t b)
instance Duplicate (Baz Map t b) where duplicate = traverse__duplicate
instance StaticLen (Baz Map t b) where staticLen = fromNatural 1
instance Len (Baz Map t b) where len = foldMap_len
instance Fold  (Baz Map t b) where foldMap  = traverse__foldMap_
instance Fold0 (Baz Map t b) where foldMap0 = traverse__foldMap_
instance Fold1 (Baz Map t b) where foldMap1 = traverse__foldMap_
instance Fold_ (Baz Map t b) where foldMap_ = traverse__foldMap_
instance Strong (Baz Map t b) where strong = map_strong
instance Map (Baz Map t b) where map = traverse_map
instance Remap (Baz Map t b) where remap _ = map

instance Traverse (Baz Pure t b) where traverse = traverse0
instance Traverse0 (Baz Pure t b) where
  traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))
instance Len (Baz Pure t b) where len = foldMap_len
instance Fold  (Baz Pure t b) where foldMap  = traverse0_foldMap0
instance Fold0 (Baz Pure t b) where foldMap0 = traverse0_foldMap0
instance Strong (Baz Pure t b) where strong = map_strong
instance Map (Baz Pure t b) where map = traverse_map
instance Remap (Baz Pure t b) where remap _ = map

instance Traverse (Baz Apply t b) where traverse = traverse1
instance Traverse1 (Baz Apply t b) where
  traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))
instance Len (Baz Apply t b) where len = foldMap_len
instance Fold  (Baz Apply t b) where foldMap  = traverse1_foldMap1
instance Fold1 (Baz Apply t b) where foldMap1 = traverse1_foldMap1
instance Strong (Baz Apply t b) where strong = map_strong
instance Map (Baz Apply t b) where map = traverse_map
instance Remap (Baz Apply t b) where remap _ = map

instance Traverse (Baz Applicative t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Applicative) (f x)))))
instance Len (Baz Applicative t b) where len = foldMap_len
instance Fold  (Baz Applicative t b) where foldMap  = traverse_foldMap
instance Strong (Baz Applicative t b) where strong = map_strong
instance Map (Baz Applicative t b) where map = traverse_map
instance Remap (Baz Applicative t b) where remap _ = map




newtype Bazaar c a b t = Bazaar {runBazaar :: forall f. c f => (a -> f b) -> f t}

sell :: forall c a b. a -> Bazaar c a b b
sell a = Bazaar (\f -> f a)

instance Strong (Bazaar Map a b) where strong = map_strong
instance Map (Bazaar Map a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Remap (Bazaar Map a b) where remap _ = map
instance Promap (Bazaar Map a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))
instance Comap (BA (Bazaar Map a) t) where comap = promap_comap

instance Strong (Bazaar Pure a b) where strong = map_strong
instance Map (Bazaar Pure a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Remap (Bazaar Pure a b) where remap _ = map
instance Pure (Bazaar Pure a b) where pure a = Bazaar (\_ -> pure a)
instance Promap (Bazaar Pure a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))
instance Comap (BA (Bazaar Pure a) t) where comap = promap_comap

instance Strong (Bazaar Apply a b) where strong = map_strong
instance Map (Bazaar Apply a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Remap (Bazaar Apply a b) where remap _ = map
instance FTimes (Bazaar Apply a b) where ftimes = ap_ftimes
instance Apply (Bazaar Apply a b) where (Bazaar mf) `ap` (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)
instance Promap (Bazaar Apply a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f  `map` k x))
instance Comap (BA (Bazaar Apply a) t) where comap = promap_comap

instance Strong (Bazaar Applicative a b) where strong = map_strong
instance Map (Bazaar Applicative a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Remap (Bazaar Applicative a b) where remap _ = map
instance Pure (Bazaar Applicative a b) where pure a = Bazaar (\_ -> pure a)
instance FTimes (Bazaar Applicative a b) where ftimes = ap_ftimes
instance Apply (Bazaar Applicative a b) where (Bazaar mf) `ap` (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)
instance Applicative (Bazaar Applicative a b)
instance Promap (Bazaar Applicative a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))
instance Comap (BA (Bazaar Applicative a) t) where comap = promap_comap
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

{-bazaar'map :: (forall f x y. c f => (x -> y) -> f x -> f y) -> (s -> t) -> Bazaar c a b s -> Bazaar c a b t-}
{-bazaar'map map = \f (Bazaar t) -> Bazaar (\afb -> f `map` t afb)-}
{-bazaar'pure :: (forall f x. c f => x -> f x) -> t -> Bazaar c a b t-}
{-bazaar'pure pure = \a -> Bazaar (\_ -> pure a)-}
