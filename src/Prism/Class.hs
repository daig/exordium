module Prism.Class (module Prism.Class, module X) where
import Dimap.Class as X
import E.Type as X
import E
import Traverse0.Class
import Traverse.Class
import K.Type

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
  prism pat constr = \p -> dimap pat (e'bifoldMap_ (\x -> x) constr) (right p)
  left :: p a b -> p (E a y) (E b y)
  left = prism (e'bifoldMap_ R (\x -> L (R x))) L
  right :: p a b -> p (E x a) (E x b)
  right = \p -> dimap e'swap e'swap (left p)

type (s ~+ a) b t = forall p. Prism p => p a b -> p s t

instance Prism (->) where
  prism pat constr f s = case pat s of
    L t -> t
    R a -> constr (f a)

{-instance Prism (Flip K) where-}
  {-left (Flip (K b)) = Flip (K (L b))-}
  {-right (Flip (K b)) = Flip (K (R b))-}
