module One (One(..),Times(..), module X) where
import Times as X
import Zero

import Ord
import Bool
import Coerce (coerce)

class Times a => One a where one :: a
timesOne :: (Eq a, One a) => a -> Bool
timesOne a = (a * one) == a && (one * a) == a

