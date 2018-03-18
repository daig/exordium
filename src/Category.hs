module Category (Category(..), module X) where
import Compose as X

class Compose p => Category p where identity :: p a a

instance Category (->) where identity = \x -> x
