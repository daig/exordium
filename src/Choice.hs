module Choice (Choice(..), (|.), (.|), module X) where
import Dimap  as X
import Sum as X (E)

class Dimap p => Choice p where
  left :: p a b -> p (E a y) (E b y)
  right :: p a b -> p (E x a) (E x b)

(.|) :: Choice p => p a b -> p (E a y) (E b y)
(.|) = left
(|.) :: Choice p => p a b -> p (E x a) (E x b)
(|.) = right
