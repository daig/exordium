module Choice (Choice(..), (|.), (.|), module X) where
import Dimap  as X
import Sum as X

class Dimap p => Choice p where
  {-# minimal prism | left | right #-}
  prism :: (s -> E t a) -> (b -> t) -> p a b -> p s t
  prism pat constr = \p -> dimap pat (either (\x -> x) constr) (right p)
  left :: p a b -> p (E a y) (E b y)
  left = prism (either R (\x -> L (R x))) L
  right :: p a b -> p (E x a) (E x b)
  right = \p -> dimap swap swap (left p)

instance Choice (->) where
  prism pat constr f s = case pat s of
    L t -> t
    R a -> constr (f a)

(.|) :: Choice p => p a b -> p (E a y) (E b y)
(.|) = left
(|.) :: Choice p => p a b -> p (E x a) (E x b)
(|.) = right
