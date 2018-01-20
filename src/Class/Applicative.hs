{-# language UndecidableInstances #-}
module Class.Applicative (module Class.Applicative, module X) where
import Class.Pure as X
import Class.Apply as X
import Class.PlusZero
import Type.K
import Type.I

class (Pure f, Apply f) => Applicative f

instance Applicative []
instance Applicative ((->) x)
instance PlusZero a => Applicative (K a)
instance Applicative I
