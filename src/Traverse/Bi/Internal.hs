module Traverse.Bi.Internal (module Traverse.Bi.Internal, module X) where
import Applicative as X
import FoldMap.Bi as X
import Coerce

data TheseK a b x = ThisK a | ThatK b | TheseK a b

thesek'biextract :: Times m => TheseK m m x -> m
thesek'biextract = \case
  ThisK a -> a
  ThatK b -> b
  TheseK a b -> a `times` b
instance Map (TheseK a b) where map _ = coerce
instance MapIso (TheseK a b) where mapIso _ _ = coerce
instance (Times a, Times b) => Apply (TheseK a b) where
  ap (TheseK a b) = \case
    ThisK a' -> TheseK (times a a') b
    ThatK b' -> TheseK a (times b b')
    TheseK a' b' -> TheseK (times a a') (times b b')
  ap (ThisK a) = \case
    ThatK b -> TheseK a b
    TheseK a' b -> TheseK (times a a') b
  ap (ThatK b) = \case
    ThatK b' -> ThatK (times b b')
    TheseK a b' -> TheseK a (times b b')
instance (One a, One b) => Pure (TheseK a b) where pure _ = one
instance (TimesOne a, TimesOne b) => Applicative (TheseK a b)
instance (One a, One b) => One (TheseK a b x) where one = TheseK one one
