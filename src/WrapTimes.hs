module WrapTimes (WrapTimes(..),module X) where
import Recip 
import Negate as X
import Coerce (coerce)

newtype WrapTimes a = Times a
instance Times a => Plus (WrapTimes a) where (+) = coerce ((*) @a)
instance One a => Def (WrapTimes a) where def = coerce (one @a)
instance Recip a => Negate (WrapTimes a) where negate = coerce (recip @a)
