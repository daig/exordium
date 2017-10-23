module First where
import Plus as X
import Times as X
import Def as X ()
import Void (__)
import Prelude (seq)

newtype First a = First a
instance Plus (First a) where a + _ = a
instance Def (First a) where
  -- | Bottom is a right identity to (+) and a zero to (*)
  def = __
instance Times (First a) where a * b = seq b a
