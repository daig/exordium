module X.Functor.Append0 (Append0,module X) where
import X.Functor.Empty as X
import X.Functor.Append as X

-- append empty a = append a empty = a
class (Empty f, Append f) => Append0 f
