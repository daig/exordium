{-# language UndecidableInstances #-}
module Applicative (Applicative, module X) where
import Pure as X
import Apply as X
import TimesOne as X
import PlusZero as X
import {-# source #-} K
import {-# source #-} I

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)

instance PlusZero a => Applicative (K a)

instance Applicative I
