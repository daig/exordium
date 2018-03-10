{-# language MagicHash #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Coerce where
import qualified Data.Coerce as C
import Unsafe.Coerce


-- | Representational type equality. Contrast with nominal equality `~`
type (#=) = C.Coercible

coerceF :: forall g f a. f a #= g a => f a -> g a
coerceF = C.coerce

coerceF# :: forall g f a. f a -> g a
coerceF# = unsafeCoerce

wrap :: forall f a. a #= f a => a -> f a
wrap = C.coerce

unwrap :: forall a f. f a #= a => f a -> a
unwrap = C.coerce

wrap# :: forall f a. a -> f a
wrap# = unsafeCoerce

unwrap# :: forall a f. f a -> a
unwrap# = unsafeCoerce

coerce :: forall b a. a #= b => a -> b
coerce = C.coerce

coerce# :: forall b a. a -> b
coerce# = unsafeCoerce

mapCoerce# :: forall b f a. f a -> f b
mapCoerce# = unsafeCoerce
