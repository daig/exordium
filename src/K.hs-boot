module K where
import Zero.Class
import One.Class
newtype K a (b :: *) = K a
instance Zero a => One (K a x)
