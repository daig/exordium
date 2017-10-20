{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Prism
  (type (+~),type (+~~)
  ,prism, prism'
  ,module X) where
import Iso as X
import I as X
import Market as X
import Sum as X
import Option as X

type (s +~ a) b t = forall p f. (Choice p, Pure f) => p a (f b) -> p s (f t)
type s +~~ a = forall p f. (Choice p, Pure f) => p a (f a) -> p s (f s)

prism :: (s -> E t a) -> (b -> t) -> (s +~ a) b t
prism seta bt = \p -> dimap seta (either pure (map bt)) (right p)

prism' :: (s -> (?) a) -> (b -> s) -> (s +~ a) b s
prism' sma = prism (\s -> case sma s of {Some a -> R a; None -> L s})

