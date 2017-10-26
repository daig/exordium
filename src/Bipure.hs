module Bipure (Bipure(..), module X) where
import Bimap as X

class Bimap p => Bipure p where
  bipure :: a -> b -> p a b
