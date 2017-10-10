module Times where
import Types
import qualified Prelude as P

class Times a where (*) :: a -> a -> a

instance Times Int where (*) = (P.*)
--TODO: add more
