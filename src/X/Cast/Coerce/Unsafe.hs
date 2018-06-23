{-# language MagicHash #-}
module X.Cast.Coerce.Unsafe where
import GHC.Prim

coerceF# :: forall g f a. f a -> g a
coerceF# = unsafeCoerce#

wrap# :: forall f a. a -> f a
wrap# = unsafeCoerce#

unwrap# :: forall a f. f a -> a
unwrap# = unsafeCoerce#

coerce# :: forall b a. a -> b
coerce# = unsafeCoerce#

mapCoerce# :: forall b f a. f a -> f b
mapCoerce# = unsafeCoerce#
