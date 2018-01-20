module Category (Category(..), module X) where
import Class.Compose as X

class Compose p => Category p where id :: p a a

instance Category (->) where id = \x -> x
