module Mapped.Internal (Context(..),Bar(..), module X) where
import Applicative.Class as X
import Distribute.Class as X

newtype Bar t b a = Bar {runBar :: forall f. (Applicative f, Distribute f) => (a -> f b) -> f t}
instance MapIso (Bar t b) where mapIso = map_mapIso
instance Map (Bar t b) where map f (Bar k) = Bar (\xfb -> k (\x -> xfb (f x)))

data Context a b t = Context (b -> t) a
instance Map (Context a b) where map f (Context bt a) = Context (\b -> f (bt b)) a
instance MapIso (Context a b) where mapIso _ = map
