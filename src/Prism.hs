{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Prism
  (type (+~),type (+~~)
  ,prism, prism'
  ,module X) where
import Iso as X
import Choice as X
import Sum as X
import Option as X

type (s +~  a) b t = forall p. Choice p => p a b -> p s t
type  s +~~ a      = forall p. Choice p => p a a -> p s s

prism' :: (s -> (?) a) -> (b -> s) -> (s +~ a) b s
prism' sma = prism (\s -> case sma s of {Some a -> R a; None -> L s})

