module Plus.F where
import List

-- | Associative: fadd (fadd a b) c = fadd a (fadd b c)
class FPlus f where fplus :: f a -> f a -> f a

instance FPlus [] where fplus = list'append
