module Plus.F where

-- | Associative: fadd (fadd a b) c = fadd a (fadd b c)
class FPlus f where fplus :: f a -> f a -> f a

instance FPlus [] where fplus = list'append

list'prepend,list'append :: [a] -> [a] -> [a]
list'prepend bs = go where
  go = \case
    [] -> bs
    a:as -> a : go as
list'append as bs = list'prepend bs as
