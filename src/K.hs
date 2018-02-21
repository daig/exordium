module K where
import Bimap.Class
import CoRMap.Class
import Comap.Class
import Applicative.Class
import PlusZero.Class
import FoldMap.Class

newtype K a (b :: *) = K a

instance Bimap K where bimap = k'bimap
k'bimap f _ = k'lmap f

instance LMap K where lmap = k'lmap
k'lmap f = \case K a -> K (f a)

instance MapIso (K a) where mapIso _ _ = k'absurd
instance Map (K a) where map _ = k'absurd
instance RMap K where rmap _ = k'absurd
instance Comap (K a) where comap _ = k'absurd
instance CoRMap K where cormap _ = k'absurd
k'absurd :: K a x -> K a y
k'absurd (K a) = K a

instance Zero a => Zero (K a b) where zero = K zero
instance Zero a => Pure (K a) where pure _ = zero
instance Plus a => Apply (K a) where K a `ap` K b = K (a `plus` b)
instance PlusZero a => Applicative (K a)

instance FoldMap (K x) where foldMap = \_ _ -> zero
