module Utils.Bazaar (module Utils.Bazaar, module X) where
import Bazaar as X

bazaar'map :: (forall f x y. c f => (x -> y) -> f x -> f y) -> (s -> t) -> Bazaar c a b s -> Bazaar c a b t
bazaar'map map = \f (Bazaar t) -> Bazaar (\afb -> f `map` t afb)
bazaar'pure :: (forall f x. c f => x -> f x) -> t -> Bazaar c a b t
bazaar'pure pure = \a -> Bazaar (\_ -> pure a)

sell :: forall c a b. a -> Bazaar c a b b
sell a = Bazaar (sell' a)

sell' :: a -> (a -> f b) -> f b
sell' a f = f a
