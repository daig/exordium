module FPlus.Class where
import List

-- | Associative: fplus (fplus a b) c = fplus a (fplus b c)
class FPlus f where fplus :: f a -> f a -> f a

instance FPlus [] where fplus = list'append
