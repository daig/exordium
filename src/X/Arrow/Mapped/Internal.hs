module X.Arrow.Mapped.Internal (Context(..),Bar(..), module X) where
import X.Functor.Applicative as X
import X.Functor.Zip as X

newtype Bar t b a = Bar {runBar :: forall f. (Applicative f, Zip f) => (a -> f b) -> f t}
instance Strong (Bar t b) where strong = map_strong
instance Map (Bar t b) where map f (Bar k) = Bar (\xfb -> k (\x -> xfb (f x)))
instance Remap (Bar t b) where remap _ = map

data Context a b t = Context (b -> t) a
instance Strong (Context a b) where strong = map_strong
instance Map (Context a b) where map f (Context bt a) = Context (\b -> f (bt b)) a
instance Remap (Context a b) where remap _ = map
