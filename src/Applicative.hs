{-# language UndecidableInstances #-}
module Applicative (Applicative, module X) where
import Pure as X
import Apply as X
import qualified Prelude as P

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)

instance Applicative f => P.Applicative f where
  pure = X.pure
  (<*>) = (|$|)
