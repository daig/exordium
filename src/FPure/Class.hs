module FPure.Class (module FPure.Class, module X) where
import FMap.Class as X

class FMap t => FPure t where fpure :: FMapC t f => f a -> t f a
