module Option (type (?)(..), module X) where
import Type.Option as X
import Class.Pure as X


instance MapIso (?) where mapIso _ = map
instance Pure (?) where pure = Some
