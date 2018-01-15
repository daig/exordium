{-# language UndecidableInstances #-}
{-# language MagicHash #-}
{-# language UndecidableSuperClasses #-}
module Forall (Forall, ForallF, inst, forall, module X) where
import Witness as X
import Coerce
import GHC.Exts (Constraint)

type family Skolem (p :: k -> Constraint) :: k where

class    p (Skolem p) => Forall (p :: k -> Constraint)
instance p (Skolem p) => Forall (p :: k -> Constraint)

class p (f a) => ComposeC (p :: k' -> Constraint) (f :: k -> k') (a :: k)
instance p (f a) => ComposeC p f a

class Forall (ComposeC p f) => ForallF (p :: k' -> Constraint) (f :: k -> k')
instance Forall (ComposeC p f) => ForallF p f

class p (t a b) => R (p :: k'' -> Constraint) (t :: k -> k' -> k'') (a :: k) (b :: k')
instance p (t a b) => R p t a b

class Forall (R p t a) => Q (p :: k'' -> Constraint) (t :: k -> k' -> k'') (a :: k)
instance Forall (R p t a) => Q p t a

class Forall (Q p t) => ForallT (p :: k''' -> Constraint) (t :: (k -> k') -> k'' -> k''')
instance Forall (Q p t) => ForallT p t

class Forall (Q p t) => ForallP (p :: k'' -> Constraint) (t :: k -> k' -> k'')
instance Forall (Q p t) => ForallP p t

-- TODO: ForallD p t ~~ forall a. p (t a a)




inst :: forall p a. Forall p =>. p a
inst = coerce# (Sub W :: Forall p =>. p (Skolem p))

forall :: forall p. (forall a. W (p a)) -> W (Forall p)
forall w = case w :: W (p (Skolem p)) of W -> W
