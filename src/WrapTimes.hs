module WrapTimes (WrapTimes(..),module X) where
import Minus as X
import Divide as X

newtype WrapTimes a = Times a
instance TimesOne a => PlusZero (WrapTimes a)
instance Times  a => Plus  (WrapTimes a) where Times a + Times b = Times (a * b)
instance Divide a => Minus (WrapTimes a) where Times a - Times b = Times (a / b)
instance One    a => Zero  (WrapTimes a) where zero              = Times one
