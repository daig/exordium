{-# language MagicHash #-}
module Prism.Class (module Prism.Class, module X) where
import Coerce
import Dimap.Class as X
import E as X
import E.Utils
import Traverse
import Star.Type
import Maybe
import K
import Comap.Class
import Re.Type

{-type Prismoid s a b t = forall f. X f => (f a -> b) -> f s -> t-}
prismoid :: (Pure g, Traverse0 f) => (s -> E t a) -> (b -> t) -> (f a -> g b) -> f s -> g t
prismoid seta bt fagb fs = case traverse0 seta fs of
  L t -> pure t
  R fa -> bt `map` fagb fa
{-prismoid' :: Traverse0 f => (s -> E t a) -> (b -> t) -> (f a -> b) -> s -> (forall r. (t -> r) -> r)-}
{-prismoid' seta bt fab fs = case seta s of-}
  {-L t -> pure t-}
  {-R fa -> bt (fab fa)-}

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
  prism pat constr = \p -> dimap pat (e'bifoldMap (\x -> x) constr) (right p)
  left :: p a b -> p (E a y) (E b y)
  left = prism (e'bifoldMap R (\x -> L (R x))) L
  right :: p a b -> p (E x a) (E x b)
  right = \p -> dimap e'swap e'swap (left p)

type (s ~+ a) b t = forall p. Prism p => p a b -> p s t

instance Prism (->) where
  prism pat constr f s = case pat s of
    L t -> t
    R a -> constr (f a)

instance Pure f => Prism (Star f) where
  prism pat constr (Star afb) = Star (\s -> case pat s of
    L t -> pure t
    R a -> constr `map` afb a)
  

{-class Choose t where-}
  {-choose :: Empty f => (f a -> f b) -> s -> t-}

unprismoid :: (forall f. Traverse0 f => (f a -> b) -> f s -> t) -> b -> t
unprismoid fabfst b = fabfst (\_ -> b) (K ())

{-ff :: ((Maybe a -> E a t) -> s -> E a t) -> s -> E a t-}
{-ff l s = (`l` s) (\case-}
  {-L a -> L a-}
  {-x@R{} -> coerce# x)-}



_Just :: (Pure g, Traverse0 f) => (f a -> g b) -> f (Maybe a) -> g (Maybe b)
_Just = (`prismoid` Just) (\case
  Just a -> R a
  Nothing -> L Nothing)

class Dimap p => Cochoice p where
  {-# minimal unleft | unright #-}
  unleft :: p (E a y) (E b y) -> p a b
  unleft p = unright (dimap e'swap e'swap p)
  unright :: p (E x a) (E x b) -> p a b
  unright p = unleft (dimap e'swap e'swap p)
{-instance Prism p -}
instance Cochoice p => Prism (Re p s t) where
  left (Re l) = Re (\p -> l (unleft p))
