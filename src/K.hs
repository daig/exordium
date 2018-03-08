module K where
import Bimap.Class
import Bitraverse_.Class
import Comap.R as X
import Applicative.Class as X
import PlusZero.Class as X

newtype K a (b :: *) = K a

instance Bimap K where bimap = k'bimap
k'bimap f _ = k'lmap f

instance MapL K where lmap = k'lmap
k'lmap f = \case K a -> K (f a)

instance MapR K where rmap _ = k'absurd
instance Comap (K a) where comap _ = k'absurd
instance ComapR K where cormap _ = k'absurd
k'absurd :: K a x -> K a y
k'absurd (K a) = K a

instance Zero a => Zero (K a b) where zero = K zero
instance Zero a => One (K a b) where one = K zero


instance BifoldMap K where bifoldMap f _ (K a) = f a
instance BifoldMap0 K where bifoldMap0 f _ (K a) = f a
instance BifoldMap1 K where bifoldMap1 f _ (K a) = f a
instance BifoldMap_ K where bifoldMap_ f _ (K a) = f a

instance Bitraverse K where bitraverse f _ (K a) = K `map` f a
instance Bitraverse0 K where bitraverse0 f _ (K a) = K `map` f a
instance Bitraverse1 K where bitraverse1 f _ (K a) = K `map` f a
