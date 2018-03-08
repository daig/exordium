module Times.WrapTimes (WrapTimes(..),module X) where
import Minus as X
import Divide as X

newtype WrapTimes a = Times a
instance TimesOne a => PlusZero (WrapTimes a)
instance Times  a => Plus  (WrapTimes a) where Times a `plus` Times b = Times (a `times` b)
instance Divide a => Minus (WrapTimes a) where Times a `minus` Times b = Times (a `divide` b)
instance One    a => Zero  (WrapTimes a) where zero              = Times one
