module K (module X) where
import Map as X
import Bimap as X
import Applicative as X
import Zero as X
import Comap as X
import Dimap as X
import Prism as X
import Sum as X (E)
import Sum
import K.Type as X
import Traverse as X
import Traverse0 as X
import FoldMap0 as X

instance Bimap K where bimap f _ (K a) = K (f a)
instance Map (K a) where map _ (K a) = K a

instance Bimap KK where bimap _ g (KK b) = (KK (g b))
instance Map (KK a) where map f (KK b) = KK (f b)

instance Prism KK where
  left (KK b) = KK (L b)
  right (KK b) = KK (R b)

instance Comap (K a) where comap _ (K a) = K a

instance Dimap KK where dimap _ f (KK a) = KK (f a)

instance Def a => Pure (K a) where pure = \_ -> K def
instance Plus a => Apply (K a) where K a |$| K b = K (a + b)
instance Zero a => Applicative (K a)

instance Traverse (K x) where traverse f (K x) = pure (K x)
instance FoldMap (K x) where foldMap = \_ _ -> def
instance FoldMap0 (K x) where foldMap0 _ _ = def
instance Traverse0 (K x) where traverse0 f (K x) = pure (K x)
