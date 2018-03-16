module Type.K where
import Map.Bi
import Traverse.Bi
import Applicative as X
import Num.Add0 as X

newtype K a (b :: *) = K a

instance Bimap K where bimap = k'bimap
k'bimap f _ = \case K a -> K (f a)

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
