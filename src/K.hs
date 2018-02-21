module K where
import Bimap.Class
import CoRMap.Class
import Comap.Class
import Applicative.Class
import PlusZero.Class
import FoldMap.Class
import Traverse0.Class as X

newtype K a (b :: *) = K a

instance Bimap K where bimap = k'bimap
k'bimap f _ = k'lmap f

instance LMap K where lmap = k'lmap
k'lmap f = \case K a -> K (f a)

instance RMap K where rmap _ = k'absurd
instance Comap (K a) where comap _ = k'absurd
instance CoRMap K where cormap _ = k'absurd
k'absurd :: K a x -> K a y
k'absurd (K a) = K a


instance Plus a => Apply (K a) where K a `ap` K b = K (a `plus` b)
instance PlusZero a => Applicative (K a)

instance FoldMap (K x) where foldMap = \_ _ -> zero

instance Traverse (K x) where traverse f (K x) = pure (K x)
