module Bipure where
import Bimap

class Bimap p => Bipure p where
  bipure :: a -> b -> p a b
