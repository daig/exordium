module Plus where
import Bool
import Ord
import Types
import qualified Prelude as P
import Coerce

{-fromInteger :: P.Num a => a-}
{-fromInteger = P.fromInteger-}

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

-- Instances --
instance Plus Natural where (+) = (P.+)
instance Plus Integer where (+) = (P.+)
instance Plus Int   where (+) = (P.+)
instance Plus Int8  where (+) = (P.+)
instance Plus Int16 where (+) = (P.+)
instance Plus Int32 where (+) = (P.+)
instance Plus Int64 where (+) = (P.+)
instance Plus Word   where (+) = (P.+)
instance Plus Word8  where (+) = (P.+)
instance Plus Word16 where (+) = (P.+)
instance Plus Word32 where (+) = (P.+)
instance Plus Word64 where (+) = (P.+)
instance Plus Bool where (+) = (P./=)

{-instance Zero Natural where zero = 0-}
{-instance Zero Integer where zero = 0-}
{-instance Zero Int   where zero = 0-}
{-instance Zero Int8  where zero = 0-}
{-instance Zero Int16 where zero = 0-}
{-instance Zero Int32 where zero = 0-}
{-instance Zero Int64 where zero = 0-}
{-instance Zero Word   where zero = 0-}
{-instance Zero Word8  where zero = 0-}
{-instance Zero Word16 where zero = 0-}
{-instance Zero Word32 where zero = 0-}
{-instance Zero Word64 where zero = 0-}
{-instance Zero Bool where zero = False-}
