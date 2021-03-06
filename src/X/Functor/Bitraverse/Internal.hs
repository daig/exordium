module X.Functor.Bitraverse.Internal (module X.Functor.Bitraverse.Internal, module X) where
import X.Functor.Applicative as X
import X.Functor.Bifold as X

data TheseK a b x = ThisK a | ThatK b | TheseK a b

thesek'biextract :: Mul m => TheseK m m x -> m
thesek'biextract = \case
  ThisK a -> a
  ThatK b -> b
  TheseK a b -> a `mul` b
instance Strong (TheseK a b) where strong = map_strong
instance Map (TheseK a b) where map _ = coerce
instance Remap (TheseK a b) where remap _ = map
instance (Mul a, Mul b) => FTimes (TheseK a b) where ftimes = ap_ftimes
instance (Mul a, Mul b) => Apply (TheseK a b) where
  ap (TheseK a b) = \case
    ThisK a' -> TheseK (mul a a') b
    ThatK b' -> TheseK a (mul b b')
    TheseK a' b' -> TheseK (mul a a') (mul b b')
  ap (ThisK a) = \case
    ThisK a' -> ThisK (mul a a')
    ThatK b -> TheseK a b
    TheseK a' b -> TheseK (mul a a') b
  ap (ThatK b) = \case
    ThisK a -> TheseK a b
    ThatK b' -> ThatK (mul b b')
    TheseK a b' -> TheseK a (mul b b')
instance (One a, One b) => Pure (TheseK a b) where pure _ = one
instance (Mul1 a, Mul1 b) => Applicative (TheseK a b)
instance (One a, One b) => One (TheseK a b x) where one = TheseK one one
