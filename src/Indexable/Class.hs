{-# language UndecidableSuperClasses #-}
module Indexable.Class (module Indexable.Class, module X) where
import Arrow.Traversed
import Optic.View
import Type.Int
import Arrow.Category
import Arrow.Mapped as X
import Prelude (($))

class (
  {-Mapping p, Mapping (Unindexed p),-}
  Unindexed (Unindexed p) ~ Unindexed p, Indexed i (Unindexed p)) => Indexed i p where
  type Unindexed p :: * -> * -> *
  type Unindexed p = p
  indexed :: p a b -> i -> Unindexed p a b
  default indexed :: (Unindexed p ~ p) => p a b -> i -> Unindexed p a b
  indexed p _ = p

at :: i -> IndexedTraversed i a a b b
at i = (`indexed` i)

instance Indexed i (->) where indexed = \a _ -> a

type IndexedTraversed i s a b t = forall p. (Indexed i p, Traversed p, Traversed (Unindexed p))
                               => p a b -> Unindexed p s t

newtype Indexing p a b = Indexing {runIndexing :: Int -> p a b}
instance (Indexed Int p, Unindexed p ~ p) => Indexed Int (Indexing p) where
  type Unindexed (Indexing p) = p
  indexed (Indexing ip) i = ip i



list'itraverse :: Applicative f => (Int -> a -> f b) -> [a] -> f [b]
list'itraverse f = go 0 where
  go i = \case
    [] -> pure []
    a:as -> (:) `map` f i a `ap` go (i `add` 1) as

list'ifoldMap :: Add0 m => (Int -> a -> m) -> [a] -> m
list'ifoldMap f = go 0 where
  go i = \case
    [] -> zero
    a:as -> f i a `add` go (i `add` 1) as

--tt :: Indexed Int p => p a m -> Unindexed p [a] m
-- tt p = go 0 where
--  go i =  unleft $ 
