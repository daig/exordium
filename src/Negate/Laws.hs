module Negate.Laws where
import Negate
import Bool
import Ord

negateNegate :: (Negate a,Eq a) => a -> Bool
negateNegate a = negate (negate a) == a
distrib :: (Eq a, Negate a) => a -> a -> Bool
distrib a b = negate (a + b) == negate a + negate b
negateZero :: (Negate a, Eq a) => a -> Bool
negateZero a = a + negate a == zero && negate a + a == zero
