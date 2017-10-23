module K (K(..),KK(..), IsK, IsKK, Def, Plus, module X) where
import Map as X
import Bimap as X
import Applicative as X
import Zero as X
import Comap as X
import Dimap as X
import Choice as X
import Sum as X (E)
import Sum

newtype K a (b :: *) = K a
instance Bimap K where bimap f _ (K a) = K (f a)
instance Map (K a) where map _ (K a) = K a

newtype KK (a :: *) b = KK b
instance Bimap KK where bimap _ g (KK b) = (KK (g b))
instance Map (KK a) where map f (KK b) = KK (f b)

instance Choice KK where
  left (KK b) = KK (L b)
  right (KK b) = KK (R b)

instance Comap (K a) where comap _ (K a) = K a

instance Dimap KK where dimap _ f (KK a) = KK (f a)

instance Def a => Pure (K a) where pure = \_ -> K def
instance Plus a => Apply (K a) where K a @$@ K b = K (a + b)
instance Zero a => Applicative (K a)

type IsK f = (Map f, Comap f)
type IsKK f = (Dimap f, Bimap f)
