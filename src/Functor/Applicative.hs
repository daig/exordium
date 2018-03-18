{-# language UndecidableInstances #-}
module Functor.Applicative (Applicative, module X) where
import Functor.Pure as X
import Functor.Apply as X
import Num.Mul1 as X
import Num.Add0 as X
import {-# source #-} Type.K
import {-# source #-} Type.I

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)

instance Add0 a => Applicative (K a)

instance Applicative I
