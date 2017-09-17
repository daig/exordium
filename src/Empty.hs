module Empty where
import Bool
import Map

class Empty f where empty :: f a
mapEmpty :: forall f a b. (Eq (f b), Map f, Empty f) => (a -> b) -> Bool
mapEmpty f = map @f f empty == empty
