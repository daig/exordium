module X.Type.I.Optics (I(..),_I,module X) where
import X.Type.I
import X.Arrow.Promap as X

_I :: Promap p => p a b -> p (I a) (I b)
_I = promap (\case I a -> a) I
