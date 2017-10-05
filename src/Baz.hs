module Baz (Baz(..),sold,module X) where
import Bazaar
import Traversable as X
import I
import O


newtype Baz t b a = Baz {runBaz :: forall f. Applicative f => (a -> f b) -> f t}
instance Map (Baz t b) where map f (Baz t) = Baz (\afb -> t (\x -> afb (f x)))
instance Traversable (Baz t b) where
  traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map sell (f x)))))
instance Foldable (Baz t b) where foldMap = foldMapDefault
sold :: Baz t a a -> t
sold m = case runBaz m I of {I t -> t}
