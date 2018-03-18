module Arrow.Mapped.Internal (Context(..),Bar(..), module X) where
import Functor.Applicative as X
import Functor.Distribute as X

newtype Bar t b a = Bar {runBar :: forall f. (Applicative f, Distribute f) => (a -> f b) -> f t}
instance Map (Bar t b) where map f (Bar k) = Bar (\xfb -> k (\x -> xfb (f x)))

data Context a b t = Context (b -> t) a
instance Map (Context a b) where map f (Context bt a) = Context (\b -> f (bt b)) a
