module ComapL where

class ComapL p where comapL :: (a -> x) -> p x b -> p a b

instance ComapL (->) where comapL f g a = g (f a)

