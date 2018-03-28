module X.Arrow.Category (Category(..), module X) where
import X.Arrow.Compose as X

class Compose p => Category p where identity :: p a a

instance Category (->) where identity = \x -> x
