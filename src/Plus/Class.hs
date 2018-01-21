module Plus.Class where
import Int.Type
import qualified Prelude as P

{-fromInteger :: P.Num a => a-}
{-fromInteger = P.fromInteger-}

-- | a + (b + c) = (a + b) + c
class Plus a where plus :: a -> a -> a


-- Instances --
instance Plus (a -> a) where f `plus` g = \a -> f (g a)
instance (Plus a, Plus b) => Plus (a,b) where (a,b) `plus` (x,y) = (plus a x,plus b y)
instance Plus Int where plus = (P.+)
{-instance Plus Natural where (+) = (P.+)-}
{-instance Plus Integer where (+) = (P.+)-}
{-instance Plus Word   where (+) = (P.+)-}
{-instance Plus Word8  where (+) = (P.+)-}
{-instance Plus Word16 where (+) = (P.+)-}
{-instance Plus Word32 where (+) = (P.+)-}
{-instance Plus Word64 where (+) = (P.+)-}
{-instance Plus Bool where (+) = (P./=)-}

