module Recip (Recip(..), module X) where
import One as X

class One a => Recip a where recip :: a -> a

