module Functor.Adjoint (module Functor.Adjoint, module X) where
import Functor.Adjoint.Class as X

{-leftAdjunct_pure :: f -| g => a -> O g f a-}
{-leftAdjunct_pure = O < leftAdjunct id-}

{-rightAdjunct_extract :: f -| g => O f g a -> a-}
{-rightAdjunct_extract = rightAdjunct id < (\(O fg) -> fg)-}

{-leftAdjunct_tabulate :: f -| g => (f () -> b) -> g b-}
{-leftAdjunct_tabulate f = leftAdjunct f ()-}

{-splitF_extractF :: f -| g => f a -> a-}
{-splitF_extractF = splitF > \case {(a,_) -> a}-}

{-splitF :: f -| g => f a -> (a,f ())-}
{-splitF = rightAdjunct ((`leftAdjunct` ()) < (,))-}
