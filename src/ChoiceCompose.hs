module ChoiceCompose (ChoiceCompose(..), module X) where 
import Choice as X
import Compose as X

class (Choice p, Compose p) => ChoiceCompose p where
  (.|.) :: p a b -> p a' b' -> p (E a a') (E b b')
  f .|. g = left f > right g
