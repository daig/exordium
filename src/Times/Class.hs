module Times.Class where
import Utils.Int
import Constraint.Type
import qualified Prelude as P

class Times a where times :: a -> a -> a

instance Times Int where times = int'times
--TODO: add more
