module X.Type.Permute where

newtype AB f a b = AB (f a b)
newtype BA f a b = BA (f b a)  -- TODO: Find general naming for type permutations

newtype ABC f a b c = ABC (f a b c)
newtype ACB f a b c = ACB (f a c b)
newtype BAC f a b c = BAC (f b a c)
newtype BCA f a b c = BCA (f b c a)
newtype CAB f a b c = CAB (f c a b)
newtype CBA f a b c = CBA (f c b a)
