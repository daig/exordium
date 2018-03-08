module One where
import {-# source #-} I
import {-# source #-} K

class One a where one :: a

instance One a => One (x -> a) where one = \_ -> one
instance One () where one = ()
instance One a => One [a] where one = [one]
instance One a => One (I a) where one = I one
--instance Zero a => One (K a x)
