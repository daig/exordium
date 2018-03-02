module Times.Class where
import Int
import GHC.Num

class Times a where times :: a -> a -> a

instance Times Int where times = (*)
--TODO: add more
