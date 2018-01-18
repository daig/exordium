module Divide (Divide(..), module X) where
import TimesOne as X

class TimesOne a => Divide a where
  {-# minimal (/) | recip #-}
  (/) :: a -> a -> a
  a / b = a * recip b
  recip :: a -> a
  recip a = one / a
