module X.Arrow.Traversed.Internal where
import X.Functor.Traverse
import X.Arrow.Promap
import X.Type.I
import X.Type.O

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
instance Fold  (Baz Map t b) where foldMap  = traverse__foldMap_
instance Fold0 (Baz Map t b) where foldMap0 = traverse__foldMap_
instance Fold1 (Baz Map t b) where foldMap1 = traverse__foldMap_
instance Fold_ (Baz Map t b) where foldMap_ = traverse__foldMap_
instance Map (Baz Map t b) where map = traverse_map

instance Traverse (Baz Pure t b) where traverse = traverse0
instance Traverse0 (Baz Pure t b) where
  traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))
instance Fold  (Baz Pure t b) where foldMap  = traverse0_foldMap0
instance Fold0 (Baz Pure t b) where foldMap0 = traverse0_foldMap0
instance Map (Baz Pure t b) where map = traverse_map

instance Traverse (Baz Apply t b) where traverse = traverse1
instance Traverse1 (Baz Apply t b) where
  traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))
instance Fold  (Baz Apply t b) where foldMap  = traverse1_foldMap1
instance Fold1 (Baz Apply t b) where foldMap1 = traverse1_foldMap1
instance Map (Baz Apply t b) where map = traverse_map

instance Traverse (Baz Applicative t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Applicative) (f x)))))
instance Fold  (Baz Applicative t b) where foldMap  = traverse_foldMap
instance Map (Baz Applicative t b) where map = traverse_map




newtype Bazaar c a b t = Bazaar {runBazaar :: forall f. c f => (a -> f b) -> f t}

sell :: forall c a b. a -> Bazaar c a b b
sell a = Bazaar (\f -> f a)

instance Map (Bazaar Map a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Promap (Bazaar Map a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))

instance Map (Bazaar Pure a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Pure (Bazaar Pure a b) where pure a = Bazaar (\_ -> pure a)
instance Promap (Bazaar Pure a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))

instance Map (Bazaar Apply a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Apply (Bazaar Apply a b) where (Bazaar mf) `ap` (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)
instance Promap (Bazaar Apply a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f  `map` k x))

instance Map (Bazaar Applicative a b) where map f (Bazaar m) = Bazaar (\k -> f `map` m k)
instance Pure (Bazaar Applicative a b) where pure a = Bazaar (\_ -> pure a)
instance Apply (Bazaar Applicative a b) where (Bazaar mf) `ap` (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)
instance Applicative (Bazaar Applicative a b)
instance Promap (Bazaar Applicative a) where promap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))
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
