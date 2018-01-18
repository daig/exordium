module Plus where

{-fromInteger :: P.Num a => a-}
{-fromInteger = P.fromInteger-}

-- | a + (b + c) = (a + b) + c
class Plus a where (+) :: a -> a -> a


-- Instances --
instance Plus (a -> a) where f + g = \a -> f (g a)
instance (Plus a, Plus b) => Plus (a,b) where (a,b) + (x,y) = (a+x,b+y)
{-instance Plus Natural where (+) = (P.+)-}
{-instance Plus Integer where (+) = (P.+)-}
{-instance Plus Word   where (+) = (P.+)-}
{-instance Plus Word8  where (+) = (P.+)-}
{-instance Plus Word16 where (+) = (P.+)-}
{-instance Plus Word32 where (+) = (P.+)-}
{-instance Plus Word64 where (+) = (P.+)-}
{-instance Plus Bool where (+) = (P./=)-}

