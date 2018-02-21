{-# language UndecidableInstances #-}
module Applicative.Class (module Applicative.Class, module X) where
import Pure.Class as X
import Apply.Class as X
import PlusZero.Class

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)
