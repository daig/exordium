module Cast where

class Cast a b where
  cast :: a -> b
