{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Prism (type (+~),type (+~~), type (+~.),type (+~~.), prism, prism', module X) where
import I as X
import Market as X
import Sum as X
import Option as X
import AllSatisfied as X

type (s +~ a) b t = forall p f. (Choice p, Pure f) => p a (f b) -> p s (f t)
type s +~~ a = forall p f. (Choice p, Pure f) => p a (f a) -> p s (f s)
type (s +~. a) b t = Market a b a (I b) -> Market a b s (I t)
type s +~~. a = Market' a a (I a) -> Market' a s (I s)

prism :: (b -> t) -> (s -> E t a) -> (s +~ a) b t
prism bt seta = \p -> dimap seta (either pure (map bt)) (right p)

prism' :: (b -> s) -> (s -> (?) a) -> (s +~ a) b s
prism' bs sma = prism bs (\s -> case sma s of {Some a -> R a; None -> L s})

class (AllSatisfied (Preserves a b) a, AllSatisfied (Preserves a b) b) => a ~? b where
  type Preserves a b :: [* -> Constraint]
  type Preserves a b = '[]
  tryCasted# :: a +~~ b
  tryCasted# = prism' upcast# downcast#
  upcast# :: b -> a
  downcast# :: a -> (?) b
