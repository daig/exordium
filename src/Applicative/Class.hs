{-# language UndecidableInstances #-}
module Applicative.Class (module Applicative.Class, module X) where
import Pure.Class as X
import Apply.Class as X
import PlusZero.Class as X

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)
