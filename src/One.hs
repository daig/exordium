module One where

class One a where one :: a

instance One a => One (x -> a) where one = \_ -> one
instance One () where one = ()
instance One a => One [a] where one = [one]
