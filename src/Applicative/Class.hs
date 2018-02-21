{-# language UndecidableInstances #-}
module Applicative.Class (module Applicative.Class, module X) where
import Pure.Class as X
import Apply.Class as X
import PlusZero.Class
import K.Type
import O.Type

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)
instance PlusZero a => Applicative (K a)
instance (Applicative f, Applicative g) => Applicative (O f g)
