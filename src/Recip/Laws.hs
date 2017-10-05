module Recip.Laws where
import Recip

recipRecip :: (Recip a,Eq a) => a -> Bool
recipRecip a = recip (recip a) == a
distrib :: (Eq a, Recip a) => a -> a -> Bool
distrib a b = recip (a * b) == recip a * recip b
recipOne :: (Recip a, Eq a) => a -> Bool
recipOne a = a * recip a == one && recip a * a == one
