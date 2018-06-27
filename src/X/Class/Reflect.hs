{-# language MagicHash #-}
module X.Class.Reflect (Reflect(..),reify,reflectResult, module X) where
import X.Cast.Coerce.Unsafe -- TODO: put them back in the same place?
import X.Kind.Type as X (Type)
import X.Kind.Constraint as X
import Data.Proxy (Proxy(..))

newtype Magic a r  = Magic  (forall (s :: Type). Reflect s a => Proxy s -> r)

class Reflect s a | s -> a  where reflect  :: proxy s -> a

reify :: forall a r. a -> (forall (s :: Type). Reflect s a => Proxy s -> r) -> r
reify a k = coerce# (Magic k :: Magic a r) (\_ -> a) Proxy

reflectResult :: forall f s a. Reflect s a => (a -> f s) -> f s
reflectResult f = f (reflect (Proxy @s))
