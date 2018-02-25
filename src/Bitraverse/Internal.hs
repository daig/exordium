module Bitraverse.Internal (module Bitraverse.Internal, module X) where
import PlusZero.Class as X
import Applicative.Class as X
import Coerce

data ThoseK a b x = NoneK | ThisK a | ThatK b | ThoseK a b
thosek'biextract = \case
  NoneK -> zero
  ThisK a -> a
  ThatK b -> b
  ThoseK a b -> a `plus` b
instance Map (ThoseK a b) where map _ = coerce
instance MapIso (ThoseK a b) where mapIso _ _ = coerce
instance (Plus a, Plus b) => Apply (ThoseK a b) where
  ap t@(ThoseK a b) = \case
    NoneK -> coerce t
    ThisK a' -> ThoseK (plus a a') b
    ThatK b' -> ThoseK a (plus b b')
    ThoseK a' b' -> ThoseK (plus a a') (plus b b')
  ap t@(ThisK a) = \case
    NoneK -> coerce t
    ThisK a' -> ThisK (plus a a')
    ThatK b -> ThoseK a b
    ThoseK a' b -> ThoseK (plus a a') b
  ap t@(ThatK b) = \case
    NoneK -> coerce t
    ThatK b' -> ThatK (plus b b')
    ThoseK a b' -> ThoseK a (plus b b')
  ap NoneK = coerce
instance Pure (ThoseK a b) where pure _ = NoneK
instance (Plus a, Plus b) => Applicative (ThoseK a b)
