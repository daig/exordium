{-# language UndecidableInstances #-}
{-# language MagicHash #-}
{-# language UndecidableSuperClasses #-}
module Forall (Forall, inst, forall, module X) where
import Witness as X
import Coerce
import GHC.Exts (Constraint)

type family Skolem (p :: k -> Constraint) :: k where

class    p (Skolem p) => Forall (p :: k -> Constraint)
instance p (Skolem p) => Forall (p :: k -> Constraint)


inst :: forall p a. Forall p =>. p a
inst = coerce# (Sub W :: Forall p =>. p (Skolem p))

forall :: forall p. (forall a. W (p a)) -> W (Forall p)
forall w = case w :: W (p (Skolem p)) of W -> W

