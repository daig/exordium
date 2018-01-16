module Prism (module Prism, module X) where
import Dimap  as X
import Sum as X
import Traverse0
import Traverse

{-type Prismoid s a b t = forall f. X f => (f a -> b) -> f s -> t-}
prismoid :: Traverse0 f => (s -> E t a) -> (b -> t) -> (f a -> b) -> f s -> t
prismoid seta bt fab fs = case traverse0 seta fs of
  L t -> t
  R fa -> bt (fab fa)

{-data List t a = Done t | More a (List t a)-}
{-instance Map (List t) where-}
  {-map f = go where go = \case {Done t -> Done t; More a as -> More (f a) (go as)}-}
{-prismoid' :: Traverse f => (s -> E t a) -> (b -> t) -> (f a -> b) -> f s -> t-}
{-prismoid' seta bt fab fs = case traverse seta fs of-}
  {-L t -> t-}
  {-R fa -> bt (fab fa)-}
  

class Dimap p => Prism p where
  {-# minimal prism | left | right #-}
  prism :: (s -> E t a) -> (b -> t) -> p a b -> p s t
  prism pat constr = \p -> dimap pat (either (\x -> x) constr) (right p)
  left :: p a b -> p (E a y) (E b y)
  left = prism (either R (\x -> L (R x))) L
  right :: p a b -> p (E x a) (E x b)
  right = \p -> dimap swap swap (left p)

type (s ~+ a) b t = forall p. Prism p => p a b -> p s t

instance Prism (->) where
  prism pat constr f s = case pat s of
    L t -> t
    R a -> constr (f a)

