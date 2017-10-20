module ComapL where

class ComapL p where comapl :: (a -> x) -> p x b -> p a b

instance ComapL (->) where comapl f g a = g (f a)

