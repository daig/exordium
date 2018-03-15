module Traverse.Bi.Internal (module Traverse.Bi.Internal, module X) where
import Applicative as X
import FoldMap.Bi as X
import Coerce

data TheseK a b x = ThisK a | ThatK b | TheseK a b

thesek'biextract :: Mul m => TheseK m m x -> m
thesek'biextract = \case
  ThisK a -> a
  ThatK b -> b
  TheseK a b -> a `mul` b
instance Map (TheseK a b) where map _ = coerce
instance (Mul a, Mul b) => Apply (TheseK a b) where
  ap (TheseK a b) = \case
    ThisK a' -> TheseK (mul a a') b
    ThatK b' -> TheseK a (mul b b')
    TheseK a' b' -> TheseK (mul a a') (mul b b')
  ap (ThisK a) = \case
    ThatK b -> TheseK a b
    TheseK a' b -> TheseK (mul a a') b
  ap (ThatK b) = \case
    ThatK b' -> ThatK (mul b b')
    TheseK a b' -> TheseK a (mul b b')
instance (One a, One b) => Pure (TheseK a b) where pure _ = one
instance (Mul1 a, Mul1 b) => Applicative (TheseK a b)
instance (One a, One b) => One (TheseK a b x) where one = TheseK one one
