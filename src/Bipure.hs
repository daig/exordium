module Bi.Pure where
import Bi.Map

class Bimap p => Bipure p where
  bipure :: a -> b -> p a b
