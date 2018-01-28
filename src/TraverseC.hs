{-# language UndecidableSuperClasses #-}
module TraverseC where
import Star.Type
import GHC.Classes (Ord)
import Maybe.Type
import Applicative.Class 
import Constraint.Type
import I.Type
import qualified Data.Map as M
{-import GHC.OverloadedLabels-}
{-import Lens.Class-}
import GHC.Types (Symbol)

class Traverse (c :: (* -> *) -> Constraint) t | t -> c where
  traverse :: c f => (a -> f b) -> t a -> f (t b)
traverse_map :: (Traverse c t, c I) => (a -> b) -> t a -> t b
traverse_map f ta = case traverse (\a -> I (f a)) ta of I tb -> tb

data NonEmpty a = Some a | More a (NonEmpty a)

instance Traverse Apply NonEmpty where
  traverse afb = \case
    Some a -> Some `map` afb a
    More a x -> More `map` afb a `ap` traverse afb x

instance Traverse Map ((,) x) where
  traverse afb (x,a) = (x,) `map` afb a

class Index i f' f' => Index i (f' :: * -> *) (f :: * -> *) | f -> f' where
  index :: f a -> i -> f' a
  default index :: f' ~ f => f a -> i -> f' a
  index fa _ = fa

class Eval f p where eval :: p a b -> a -> f b

instance Index i ((,) x) ((,) x)
instance Index i I I
instance Index i Maybe Maybe

instance Ord k => Index k Maybe (M.Map k) where
  index m k = M.lookup k m
instance Index a I ((->) a) where index f a = I (f a)
