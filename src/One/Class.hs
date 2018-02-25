module One.Class where
import Zero.Class

class One a where one :: a

instance One a => One (x -> a) where one = \_ -> one
instance One () where one = ()
instance One a => One [a] where one = [one]
