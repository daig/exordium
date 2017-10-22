module Choice (Choice(..), (|.), (.|), module X) where
import Dimap  as X
import Sum as X

class Dimap p => Choice p where
  {-# minimal prism | left | right #-}
  prism :: (b -> t) -> (s -> E t a) -> p a b -> p s t
  prism constr pat = \p -> dimap pat (either (\x -> x) constr) (right p)
  left :: p a b -> p (E a y) (E b y)
  left = prism L (either R (\x -> L (R x)))
  right :: p a b -> p (E x a) (E x b)
  right = \p -> dimap swap swap (left p)

instance Choice (->) where
  prism constr pat f s = case pat s of
    L t -> t
    R a -> constr (f a)

(.|) :: Choice p => p a b -> p (E a y) (E b y)
(.|) = left
(|.) :: Choice p => p a b -> p (E x a) (E x b)
(|.) = right
