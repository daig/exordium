module K where
import Zero
import One
newtype K a (b :: *) = K a
instance Zero a => One (K a x)
