module Traversed (module Traversed, module X) where
import {-# source #-} I
import {-# source #-} K
import Traverse as X
import Traversed.Internal
import Prism.Class as X
import Swap
import E.Utils
import Applicative.Class as X
import Star.Type

class (Traversed0 p, Traversed1 p) => Traversed p where
  {-# minimal traversal | traversed #-}
  traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Applicative) (traversed pab)
  traversed :: Traverse t => p a b -> p (t a) (t b)
  traversed = traversal traverse


traversed_dimap :: Traversed p => (a -> x) -> (y -> b) -> p x y -> p a b
traversed_dimap f g = traversal (\xfy a -> map g (xfy (f a)))

traverseOf :: Applicative f => (Star f a b -> Star f s t) -> (a -> f b) -> s -> f t
traverseOf l afb s = case l (Star afb) of Star sft -> sft s

traversed0_left :: Traversed0 p => p a b -> p (E a y) (E b y)
traversed0_left = \p -> dimap e'swap e'swap (traversed0 p)
traversed0_right :: Traversed0 p => p a b -> p (E x a) (E x b)
traversed0_right = traversed0

--itraverseOf :: Applicative f => (IndexingP (Star f) a b -> IndexingP (Star f) s t) -> (Int -> a -> f b) -> s -> f t
--itraverseOf l iafb = runStar (indexP (l (IndexingP (\i -> (i `plus` (1::Int),Star (iafb i))))) (0::Int))

--itraverseOf' :: (IFun f Int a b -> IFun f Int s t) -> (Int -> a -> f b) -> s -> f t
--itraverseOf' l iafb = case l (IFun iafb) of IFun isft -> isft 0

{-type (s *~. a) b t = (a -> Pretext (->) a b b) -> (s -> Pretext (->) a b t)-}
{-type s *~~. a = (a -> Pretext (->) a a a) -> (s -> Pretext (->) a a s)-}

{-type (s ~*.  a) b t = A Traversed_ a b a b -> A Traversed_ a b s t-}
{-type  s ~**. a = A Traversed_ a a a a -> A Traversed_ a a s s-}

{-withTraversed_ :: (s ~*. a) b t -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_ l f = case l (A Traversed_ (\x -> x) (\_ b -> b)) of A Traversed_ x y -> f x y-}

{-cloneTraversed_ :: (s ~*. a) b t -> (s ~* a) b t-}
{-cloneTraversed_ l = withTraversed_ l (\x y p -> lens x y p)-}

{-withTraversed_' :: (forall f. Map f => (a -> f b) -> s -> f t) -> ((s -> a) -> (s -> b -> t) -> r) -> r-}
{-withTraversed_' -}

type (s ~*  a) b t = forall p. Traversed_ p => p a b -> p s t

($:) :: Traversed_ p => p a (b -> c) -> p (a,b) c
($:) = \p -> (\(f,x) -> f x) `rmap` first p


{-ff :: (s -> FunList a b t) -> (forall f. Apply f => (a -> f b) -> s -> f t)-}
{-ff sabt afb s = case sabt s of-}
  {-Done t -> gt-}
class Traversed_ p => Traversed1 p where
  {-# minimal traversal1 | traversed1 #-}
  {-funList :: (s -> FunList a b t) -> p a b -> p s t-}
  traversal1 :: (forall f. Apply f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal1 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Apply) (traversed1 pab)
  traversed1 :: Traverse1 t => p a b -> p (t a) (t b)
  traversed1 = traversal1 traverse1


{-type (s @!~ a) b t = forall f. Apply f => (a -> f b) -> s -> f t-}
{-type s @!~~ a      = forall f. Apply f => (a -> f a) -> s -> f s-}

class (Prism p, Traversed_ p) => Traversed0 p where
  traversal0 :: (forall f. Pure f => (a -> f b) -> s -> f t) -> p a b -> p s t
  {-traversal0 f pab = dimap (\s -> Baz (\afb -> f afb s)) (sold @Pure) (traversed0 pab)-}
  traversed0 :: Traverse0 t => p a b -> p (t a) (t b)
  traversed0 = traversal0 traverse0
  lens0 :: (s -> E t a) -> (s -> b -> t) -> p a b -> p s t
  lens0 get set pab = dimap (\s -> (get s, s)) (\(bt, s) -> e'bifoldMap (\x -> x) (set s) bt) (first (right pab))


{-type (s @?~ a) b t  = forall f. Pure f => (a -> f b) -> s -> f t-}
{-type s @?~~ a       = forall f. Pure f => (a -> f a) -> s -> f s-}

class Dimap p => Traversed_ p where
  {-# minimal lens | traversal_ | traversed_ | first | second #-}
  lens :: (s -> a) -> (s -> b -> t) -> p a b -> p s t
  lens get set = \p -> dimap (\x -> (x,get x)) (\(s,b) -> set s b) (second p)
  {-lens get set = traversal_ (\afb s -> set s `map` afb (get s))-}
  traversal_ :: (forall f. Map f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traversal_ f = lens (\s -> case f K s of {K a -> a}) (\s b -> case f (\_ -> I b) s of {I t -> t})
  traversed_ :: Traverse_ t => p a b -> p (t a) (t b)
  traversed_ = traversal_ traverse_
  second :: p a b -> p (x,a) (x,b)
  {-second = \p -> dimap swap swap (first p)-}
  second = traversed_
  first :: p a b -> p (a,y) (b,y)
  {-first = lens (\(a,_) -> a) (\(_,c) b -> (b,c))-}
  {-first = traversal_ (\afb (a,y) -> (,y) `map` afb a)-}
  first = \p -> dimap swap swap (second p)

traversal__dimap :: Traversed_ p => (a -> x) -> (y -> b) -> p x y -> p a b
traversal__dimap f g = traversal_ (\xfy a -> map g (xfy (f a)))


instance Map f => Traversed_ (Star f) where traversal_ afbsft (Star afb) = Star (\s -> afbsft afb s)
{-instance Optic Traversed_ where data A Traversed_ a b s t = Traversed_ (s -> a) (s -> b -> t)-}
{-instance Traversed_ (A Traversed_ a b) where-}
  {-first (Traversed_ x y) = Traversed_ (\(a,_) -> x a) (\(s,c) b -> (y s b,c))-}
{-instance Dimap (A Traversed_ a b) where-}
  {-dimap f g (Traversed_ x y) = Traversed_ (\s -> x (f s)) (\s b -> g (y (f s) b))-}

{-data A Traversed_ p a b t = Pretext {runPretext :: forall f. Map f => p a (f b) -> f t}-}

instance Traversed_ (->) where
  lens sa sbt ab = \s -> sbt s (ab (sa s))
  first f (a,x) = (f a,x)
  second f (x,a) = (x, f a)
instance Traversed1 (->) where traversal1 l f s = case l (\a -> I (f a)) s of {I t -> t}
instance Traversed0 (->) where traversal0 l f s = case l (\a -> I (f a)) s of {I t -> t}
instance Traversed (->) where traversal l f s = case l (\a -> I (f a)) s of {I t -> t}

instance Apply f => Traversed1 (Star f) where traversal1 afbsft (Star afb) = Star (\s -> afbsft afb s)
instance Pure f => Traversed0 (Star f) where traversal0 afbsft (Star afb) = Star (\s -> afbsft afb s)
instance Applicative f => Traversed (Star f) where traversal afbsft (Star afb) = Star (\s -> afbsft afb s)
