module Class.Divide (module Class.Divide, module X) where
import Class.TimesOne as X

class TimesOne a => Divide a where
  {-# minimal divide | recip #-}
  divide :: a -> a -> a
  a `divide` b = a `times` recip b
  recip :: a -> a
  recip a = one `divide` a
