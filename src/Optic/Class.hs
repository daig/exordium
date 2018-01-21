{-# language UndecidableSuperClasses #-}
module Optic.Class (module Optic.Class, module X) where
import Constraint.Type as X
import Forall2.Class as X

class Forall2F c (A c) => Optic (c :: (k -> k' -> *) -> Constraint)  where
  data A c :: k -> k' -> k -> k' -> *
  optic :: c p => A c a b s t -> p a b -> p s t
