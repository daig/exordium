module Empty.Laws where
import Empty
import Map
import Bool

mapEmpty :: forall f a b. (Eq (f b), Map f, Empty f) => (a -> b) -> Bool
mapEmpty f = map @f f empty == empty
