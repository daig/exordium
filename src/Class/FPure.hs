module Class.FPure (module Class.FPure, module X) where
import Class.FMap as X

class FMap t => FPure t where fpure :: FMapC t f => f a -> t f a
