module Choice where
import Dimap
import Sum

class Dimap p => Choice p where
  left :: p a b -> p (E a y) (E b y)
  right :: p a b -> p (E x a) (E x b)
