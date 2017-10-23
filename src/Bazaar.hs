module Bazaar (Bazaar(..),sell,module X) where
import RelTraverse as X
import Dimap as X
import Prelude (($)) -- TOOD reexport

-- TODO: clean up instances??
newtype Bazaar c a b t = Bazaar {runBazaar :: forall f. c f => (a -> f b) -> f t}
instance Map (Bazaar Map a b) where map f (Bazaar t) = Bazaar (\afb -> f $@ t afb)
instance Dimap (Bazaar Map a) where dimap f g (Bazaar m) = Bazaar (\k -> g $@ m (\x -> f $@ k x))
instance Map (Bazaar Pure a b) where map f (Bazaar t) = Bazaar (\afb -> f $@ t afb)
instance Pure (Bazaar Pure a b) where pure a = Bazaar (\_ -> pure a)
instance Dimap (Bazaar Pure a) where dimap f g (Bazaar m) = Bazaar (\k -> g $@ m (\x -> f $@ k x))
instance Map (Bazaar Apply a b) where map f (Bazaar t) = Bazaar (\afb -> f $@ t afb)
instance Apply (Bazaar Apply a b) where (Bazaar mf) @$@ (Bazaar ma) = Bazaar (\k -> mf k @$@ ma k)
instance Dimap (Bazaar Apply a) where dimap f g (Bazaar m) = Bazaar (\k -> g $@ m (\x -> f  $@ k x))
instance Map (Bazaar Applicative a b) where map f (Bazaar t) = Bazaar (\afb -> f $@ t afb)
instance Pure (Bazaar Applicative a b) where pure a = Bazaar (\_ -> pure a)
instance Apply (Bazaar Applicative a b) where (Bazaar mf) @$@ (Bazaar ma) = Bazaar (\k -> mf k @$@ ma k)
instance Applicative (Bazaar Applicative a b)
instance Dimap (Bazaar Applicative a) where dimap f g (Bazaar m) = Bazaar (\k -> g $@ m (\x -> f $@ k x))
sell :: forall c a b. a -> Bazaar c a b b
sell a = Bazaar ($ a)

