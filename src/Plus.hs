module Plus where
import Bool
import Ord

class Plus a where (+) :: a -> a -> a
assoc :: (Eq a, Plus a) => a -> a -> a -> Bool
assoc a b c = (a+b)+c == a+(b+c)

class Plus a => Zero a where zero :: a
plusZero :: (Eq a, Zero a) => a -> Bool
plusZero a = (a + zero) == a && (zero + a) == a

class Zero a => Negate a where negate :: a -> a
negateNegate :: (Negate a,Eq a) => a -> Bool
negateNegate a = negate (negate a) == a
distrib :: (Eq a, Negate a) => a -> a -> Bool
distrib a b = negate (a + b) == negate a + negate b
negateZero :: (Negate a, Eq a) => a -> Bool
negateZero a = a + negate a == zero && negate a + a == zero

