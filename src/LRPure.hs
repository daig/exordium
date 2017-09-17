module LRPure where
import Bimap

class Bimap p => LRPure p where
  inL :: a -> p a b
  inR :: b -> p a b
