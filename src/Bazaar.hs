module Bazaar (Bazaar(..),sell,module X) where
import RelTraverse as X
import Dimap as X
import Applicative as X

newtype Bazaar a b t = Bazaar {runBazaar :: forall f. Applicative f => (a -> f b) -> f t}
instance Map (Bazaar a b) where map f (Bazaar t) = Bazaar (\afb -> map f (t afb))
instance Pure (Bazaar a b) where pure a = Bazaar (\_ -> pure a)
instance Apply (Bazaar a b) where (Bazaar mf) |@| (Bazaar ma) = Bazaar (\k -> mf k |@| ma k)
instance Dimap (Bazaar a) where dimap f g (Bazaar m) = Bazaar (\k -> map g (m (\x -> map f (k x))))
instance ComapL (Bazaar a) where comapL = premap
instance MapR (Bazaar a) where mapR = postmap
sell :: a -> Bazaar a b b
sell a = Bazaar (\k -> k a)

