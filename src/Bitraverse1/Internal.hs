module Bitraverse1.Internal (module Bitraverse1.Internal, module X) where
import Applicative.Class as X
import Coerce

data TheseK a b x = ThisK a | ThatK b | TheseK a b
thesek'biextract = \case
  ThisK a -> a
  ThatK b -> b
  TheseK a b -> a `plus` b
instance Map (TheseK a b) where map _ = coerce
instance MapIso (TheseK a b) where mapIso _ _ = coerce
instance (Plus a, Plus b) => Apply (TheseK a b) where
  ap (TheseK a b) = \case
    ThisK a' -> TheseK (plus a a') b
    ThatK b' -> TheseK a (plus b b')
    TheseK a' b' -> TheseK (plus a a') (plus b b')
  ap (ThisK a) = \case
    ThatK b -> TheseK a b
    TheseK a' b -> TheseK (plus a a') b
  ap (ThatK b) = \case
    ThatK b' -> ThatK (plus b b')
    TheseK a b' -> TheseK a (plus b b')
instance (Zero a, Zero b) => Pure (TheseK a b) where pure _ = ThisK zero
instance (PlusZero a, PlusZero b) => Applicative (TheseK a b)
