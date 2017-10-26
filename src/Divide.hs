module Divide (Divide(..), module X) where
import One as X

class One a => Divide a where (/) :: a -> a -> a
