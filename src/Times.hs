{-# language UndecidableSuperClasses #-}
module Times where
import Int
import Types (Constraint)
import qualified Prelude as P

class Times a where
  type TimesC a :: Constraint
  type TimesC a = ()
  (*) :: a -> a -> a

instance Times Int where (*) = (P.*)
--TODO: add more

newtype WrapTimes a = WrapTimes a
instance Times a => Plus (WrapTimes a) where WrapTimes a + WrapTimes b = WrapTimes (a*b)
