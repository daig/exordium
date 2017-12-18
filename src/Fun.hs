{-^ LANGUAGE QuasiQuotes ^-}
module Fun (module Fun, module X, type TYPE) where 

{-import GHC.Base as X (($))-}
import GHC.Prim (TYPE,seq)
import GHC.Types as X (RuntimeRep(..))
import GHC.Conc (par,pseq)
import qualified Prelude as P
import Prelude (fromIntegral, length,sum,(/),(-))

(^)    :: (a -> b) -> a -> b
(^!)   :: (a -> b) -> a -> b
(.^)   :: (a -> b -> c) -> b -> a -> c
(.^!)  :: (a -> b -> c) -> b -> a -> c
(..^)  :: (a -> b -> c -> d) -> c -> a -> b -> d
(..^!) :: (a -> b -> c -> d) -> c -> a -> b -> d
f  ^   a      = f a
(f .^  b) a   = f a b
(f ..^ c) a b = f a b c
(^!)   f !a     = f a
(.^!)  f !b a   = f a b
(..^!) f !c a b = f a b c

flip f = \b a -> f a b

f ^: (a,b) = f a b
(^!:) f (!a,!b) = f a b
(^.) :: P.Functor f => (a -> r) -> f a -> f r
(^.) = P.fmap
{-(f ^. a) b = f (a,b)-}
{-(^!.) f !a = \b -> f (a,b)-}

(!)  :: a -> b -> a
(!!) :: a -> b -> b
(!) a _ = a
(!!) = pseq
(~!) = par

(.) :: a -> (a -> r) -> r
a . f = f a
a &. f = f a a

(!.) !a f = f a

(&>) :: (a -> a -> x) -> (x -> b) -> a -> b
f &> g = \a -> g (f a a)
(<&) :: (x -> b) -> (a -> a -> x) -> a -> b
g <& f = f &> g
(&<) :: (x -> x -> r) -> (a -> x) -> a -> r
f &< g = ((f << g) ^&)
(>&) :: (a -> x) -> (x -> x -> r) -> a -> r
g >& f = f &< g
f << g = \a b -> f (g a) (g b) -- delete
f ^& a = f a a

(<) :: (x -> r) -> (a -> x) -> a -> r
(f < g) a = f (g a)
(>) :: (a -> x) -> (x -> r) -> a -> r
(g > f) a = f (g a)
(.<) :: (a -> x -> r) -> (b -> x) -> a -> b -> r
f .< g = \a b -> f a (g b)
g >. f = \a b -> f a (g b)
(..<) :: (a -> b -> x -> r) -> (c -> x) -> a -> b -> c -> r
f ..< g = \a b c -> f a b (g c)

(<.) :: (x -> r) -> (a -> b -> x) -> a -> b -> r
f <. g = \a b -> f (g a b)
(.>) :: (a -> b -> x) -> (x -> r) -> a -> b -> r
g .> f = \a b -> f (g a b)

(f .>| h) g = (f .><. g) h
(|<.) :: ((c -> d -> y) -> a -> b -> c -> d -> r) -> (c -> d -> y) -> a -> b -> c -> d -> r
(|<.) = (^)
(.><.) :: (a -> b -> x) -> (c -> d -> y) -> (x -> y -> r) -> a -> b -> c -> d -> r
(f .><. g) h = \a b x y -> h (f a b) (g x y)
xxx f g h = f .> h <. g

(<@) :: (x -> a -> r) -> (a -> x) -> a -> r
{-f <@ g = \a -> f (g a) a-}
f <@ g = (f<g ^&)
f .^. g = \a -> f a (g a)
(@<) = (.^.)
f >@ g = g <@ f
(@>) = (...)
g ... f = f .^. g
(>&<) :: (a -> x) -> (a -> y) -> (x -> y -> r) -> a -> r
(f >&< g) h = \a -> h (f a) (g a)
(f >| h) g = (f >&< g) h
(|<) = (^)
(f >@< g) h = \a -> h (f a) a (g a) -- delete?
(f >.< g) h = f > h .< g
infixl 9 <,.<
infixl 8 >, >.
infixl 7 >&<, ^,.^.,^&, <@, @<, >|, |<
infixl 6 >@, @>
infixl 1 ., &., ...


mean :: [P.Double] -> [P.Double]
{-mean t = sum t / fromIntegral (length t)-}
{-mean = sum >|(/)|< fromIntegral<length-}
     {-@> \as n -> (\a->a-n) ^. as-}
mean = sum >&< fromIntegral<length
     ^     (/)
     @> \as n -> (\a->a-n) ^. as
{-test :: (P.Fractional a, P.Functor t, P.Foldable t) => t a -> a-}
test = length>fromIntegral @>(/)< sum  >&<  length>fromIntegral

f :: P.String -> P.Int -> ()
f a b = ()


-- Why left associative
-- https://www.mail-archive.com/haskell-cafe@haskell.org/msg12549.html

class Moduled m where
  type Module m
  (>>) :: Module m -> m -> m

{-instance P.Monad m => Moduled (m a) where-}
  {-type Module (m a) = m ()-}
  {-(>>) = (P.>>)-}
