module K where
import Map
import Bimap
import Void

newtype K a b = K a
instance Bimap K where bimap f _ (K a) = K (f a)
instance Map (K a) where map _ (K a) = K a

newtype KK a b = KK b
instance Bimap KK where bimap _ g (KK b) = (KK (g b))
instance Map (KK a) where map f (KK b) = KK (f b)
