{-# language UndecidableSuperClasses #-}
module Class.Optic (module Class.Optic, module X) where
import Type.Constraint as X
import Class.Forall2 as X

class Forall2F c (A c) => Optic (c :: (k -> k' -> *) -> Constraint)  where
  data A c :: k -> k' -> k -> k' -> *
  optic :: c p => A c a b s t -> p a b -> p s t
