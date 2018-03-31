module X.Functor.Append where

-- | Associative: fadd (fadd a b) c = fadd a (fadd b c)
class Append f where append :: f a -> f a -> f a

instance Append [] where append = list'append

list'prepend,list'append :: [a] -> [a] -> [a]
list'prepend bs = go where
  go = \case
    [] -> bs
    a:as -> a : go as
list'append as bs = list'prepend bs as
