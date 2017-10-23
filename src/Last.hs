module Last where
import Plus as X
import Times as X
import Def as X ()
import Void (__)
import Prelude (seq)

newtype Last a = Last a
instance Plus (Last a) where _ + b = b
instance Def (Last a) where
  -- | Bottom is a left identity to (+) and a zero to (*)
  def = __
instance Times (Last a) where (*) = seq
