module Witness (W(..), type (=>.)(..), module X) where
import Constraint as X
import Profunctor as X
import Prelude 

data W c = c => W
newtype c |=> r = WithCtx (c => r)
instance Profunctor (|=>) where
  type Precat (|=>) = (=>.)
  premap' p (WithCtx cr) = WithCtx (comapEnv p cr)
  type Postcat (|=>) = (->)
  postmap' f (WithCtx cr) = WithCtx (f cr)
  dimap' p f (WithCtx cr) = WithCtx (comapEnv p (f cr))

withWitness :: W c -> (c => r) -> r
withWitness W = \r -> r
-- | A value @a =>. b@ is a witness to the fact that forall x, a x implies b x
data a =>. b = Sub (a => W b)
-- | Contravariantly map an entailment over an environment
comapEnv :: a =>. b -> (b => r) -> (a => r)
comapEnv w r = case w of Sub W -> r
infixr `comapEnv`
instance Compose (=>.) where f < g = Sub (g `comapEnv` f `comapEnv` W)
instance Category (=>.) where id = Sub W

-- | A @CFun c k a b@ is a function from @a@ to @b@, requiring constraint @c@ and providing constraint @k@
newtype CFun c k a b = CFun {runCFun :: c => (k => a) -> b}
purecf :: (c a => a -> b) -> CFun (c a) (c a) a b
purecf f = CFun f
purek :: k => CFun c k a a
purek = CFun (\a -> a)
purea :: a -> CFun c c x a
purea a = CFun (\_ -> a)
puref :: (c => a -> b) -> CFun c c a b
puref f = CFun f

testDyn :: Show a => a -> CFun c (Show x) (x -> y) y
testDyn a = CFun (\f -> f a)

teste = puref @(Show Int) @Int show
testa = purea @Int 3
testf :: CFun c c Int Int
testf = puref (*2)
{-compk :: CFun c k a a -> CFun k q a a -> CFun c (k,q) a a -}
{-compk (CFun f) (CFun g) = CFun (f < g)-}
(><) :: CFun k q x y -> CFun c k y z -> CFun c q x z
CFun f >< CFun g = CFun (\x -> g (f x))

postmapC :: (c => y -> z) -> CFun c k x y -> CFun c k x z
postmapC f (CFun g) = CFun (\x -> f (g x))

{-f :: c => (k => x) -> y-}
{-g :: k => (q => y) -> z-}
{-c => (q => x) -> z-}
