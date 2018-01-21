module K (module K, module X) where
import K.Type as X

k'bimap f _ = k'lmap f
k'lmap f = \case K a -> K (f a)
k'map _ = k'absurd
k'absurd :: K a x -> K a y
k'absurd (K a) = K a
