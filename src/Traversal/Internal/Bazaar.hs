module Traversal.Internal.Bazaar where
import Applicative.Class
import Dimap
import I.Type
import O.Type
import Instances

import Language.Haskell.TH (runIO)
import Prelude ((>>),print)

runIO (print "Bazaar") >> syncTH

newtype Bazaar c a b t = Bazaar {runBazaar :: forall f. c f => (a -> f b) -> f t}

sell :: forall c a b. a -> Bazaar c a b b
sell a = Bazaar (\f -> f a)
syncTH

[instances| Dimap (Bazaar Pure a) where
  dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))|]
[instances| Pure (Bazaar Pure a b) where pure a = Bazaar (\_ -> pure a)|]

[instances| Dimap (Bazaar Apply a) where
  dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f  `map` k x))|]
[instances| Apply (Bazaar Apply a b) where
  ap (Bazaar mf) (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)|]

[instances| Dimap (Bazaar Applicative a) where
  dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))|] 
[instances| Applicative (Bazaar Applicative a b) where
  pure a = Bazaar (\_ -> pure a)
  ap (Bazaar mf) (Bazaar ma) = Bazaar (\k -> mf k `ap` ma k)|]

[instances| Dimap (Bazaar Map a) where
  dimap f g (Bazaar m) = Bazaar (\k -> g `map` m (\x -> f `map` k x))|]
[instances| Map (Bazaar Map a b)|]
