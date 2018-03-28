{-# language UndecidableInstances #-}
module X.Functor.Applicative (Applicative, module X) where
import X.Functor.Pure as X
import X.Functor.Apply as X
import X.Num.Mul1 as X
import X.Num.Add0 as X
import {-# source #-} X.Type.K
import {-# source #-} X.Type.I
import X.Type.IO

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)

instance Add0 a => Applicative (K a)
instance Add0 a => Applicative ((,) a)

instance Applicative I
instance Applicative IO
