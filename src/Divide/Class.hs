module Divide.Class (module Divide.Class, module X) where
import TimesOne.Class as X

class TimesOne a => Divide a where
  {-# minimal divide | recip #-}
  divide :: a -> a -> a
  a `divide` b = a `times` recip b
  recip :: a -> a
  recip a = one `divide` a
