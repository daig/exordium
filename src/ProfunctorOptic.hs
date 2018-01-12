{-# language UndecidableSuperClasses #-}
module ProfunctorOptic where
import Strong as X
import Choice as X
import Sum as X
import K as X
import BiComap as X

newtype ForgetE r a b = ForgetE {runForgetE :: a -> E b r}

type AffTraversal s a b t = forall p. (Strong p, Choice p) => p a b -> p s t
affTraversal :: (s -> E t a) -> (s -> b -> t) -> AffTraversal s a b t
affTraversal get set pab = dimap (\s -> (get s, s)) (\(bt, s) -> either (\x -> x) (set s) bt) (first (right pab))
