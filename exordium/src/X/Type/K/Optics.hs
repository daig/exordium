module X.Type.K.Optics (K(..),_K,module X) where
import X.Type.K
import X.Arrow.Promap as X

_K :: Promap p => p a b -> p (K a x) (K b x)
_K = promap (\case K a -> a) K
