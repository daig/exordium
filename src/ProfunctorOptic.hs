{-# language UndecidableSuperClasses #-}
module ProfunctorOptic where
import Strong as X
import Choice as X
import Sum as X
import K as X
import BiComap as X

class Trivial a
instance Trivial a
class (f a, g a) => (f :/\: g) a
instance (f a, g a) => (f :/\: g) a

type Optic c s a b t = forall p. c p => p a b -> p s t

type Equality s a b t = forall p. p a b -> p s t

type Iso s a b t = forall p. Dimap p => p a b -> p s t
iso :: (s -> a) -> (b -> t) -> Iso s a b t
iso = dimap

type Lens s a b t = forall p. Strong p => p a b -> p s t

type Prism s a b t = forall p. Choice p => p a b -> p s t
prism :: (b -> t) -> (s -> E t a) -> Prism s a b t
prism set get pab = dimap get (either (\x -> x) set) (right pab)

newtype ForgetE r a b = ForgetE {runForgetE :: a -> E b r}

type PrimReview s a b t = forall p. Bimap p => p a b -> p s t
type Review t b = forall p. (Choice p, Bimap p) => p b b -> p t t
upto :: (b -> t) -> Review t b
upto f = bimap f f
upto' :: (b -> t) -> Review t b
upto' f p = premap (\_ -> ()) (lmap (\_ -> ()) (postmap f p))
uptoP :: (a -> s) -> (b -> t) -> PrimReview s a b t
uptoP = bimap

reviewP :: (KK a b -> KK s t) -> b -> t
reviewP o b = (\(KK x) -> x) (o (KK b))

type PrimGetter s a b t = forall p. BiComap p => p a b -> p s t
type Getter s a = forall p. (Strong p, BiComap p) => p a a -> p s s

to :: (s -> a) -> PrimGetter s a a s
to f = bicomap f f
toP :: (s -> a) -> (t -> b) -> PrimGetter s a b t
toP = bicomap


newtype Forget r a b = Forget {runForget :: a -> r}
viewP :: (Forget a a b -> Forget a s t) -> s -> a
viewP o = runForget (o (Forget (\x -> x)))

type AffTraversal s a b t = forall p. (Strong p, Choice p) => p a b -> p s t
affTraversal :: (s -> E t a) -> (s -> b -> t) -> AffTraversal s a b t
affTraversal get set pab = dimap (\s -> (get s, s)) (\(bt, s) -> either (\x -> x) (set s) bt) (first (right pab))
