{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Plus (Plus(..), module X) where
import Bool
import Def as X
import Ord
import Types
import qualified Prelude as P
import Coerce
import Trivial

{-fromInteger :: P.Num a => a-}
{-fromInteger = P.fromInteger-}

class Plus a where (+) :: a -> a -> a
assoc :: (Eq a, Plus a) => a -> a -> a -> Bool
assoc a b c = (a+b)+c == a+(b+c)



-- Instances --
instance Plus (a -> a) where f + g = \a -> f (g a)
instance Plus Natural where (+) = (P.+)
instance Plus Integer where (+) = (P.+)
instance Plus Int where (+) = (P.+)
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

instance (Plus a, Plus b) => Plus (a,b) where (a,b) + (x,y) = (a+x,b+y)
