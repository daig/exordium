{-# language TypeInType #-}
module Free where
import Kind.Constraint
import Constraint.Trivial
import NatTrans
import Kind.Type
import Monad

  

{-class Freely (c :: k -> Constraint) where-}
  {-data Free c :: k -> k-}
  {-liftFree :: c f => f a -> Free c f a-}
  {-lowerFree :: Free c f a -> f a-}

{-data family Free (c :: k -> Constraint) :: k -> k-}
{-data family CoFree (c :: k -> Constraint) :: k -> k-}

{-class Free1 (c :: (k -> *) -> Constraint) where-}
  {-liftFree1 :: c f => f a -> Free c f a-}
  {-lowerFree1 :: Free c f a -> f a-}

{-class CoFree1 (c :: (k -> *) -> Constraint) where-}
  {-liftCoFree1 :: f a -> CoFree c f a-}
  {-lowerCoFree1 :: c f => CoFree c f a -> f a-}

{-class Free2 (c :: (k -> k' -> *) -> Constraint) where-}
  {-liftFree2 :: c p => p a b -> Free c p a b-}
  {-lowerFree2 :: Free c p a b -> p a b-}

{-class (forall x. c (f x) => c' (t f x)) => Free (c :: (k -> *) -> Constraint) (c' :: (k -> *) -> Constraint) (t :: (k -> *) -> k -> *) where-}
class Free (c :: (k -> *) -> Constraint) where
  type Free'C c :: (k -> *) -> Constraint
  type Free'C c = Trivial
  data T c :: (k -> *) -> k -> *
  lift :: c f => f a -> T c f a
  lower :: Free'C c f => T c f a -> f a

class CoFree (c :: (k -> *) -> Constraint) where
  type CoFree'C c :: (k -> *) -> Constraint
  type CoFree'C c = Trivial
  data CoT c :: (k -> *) -> k -> *
  liftc :: c f => f a -> CoT c f a
  lowerc :: CoFree'C c f => CoT c f a -> f a

{-
class Lift c t | t -> c where lift' :: c f => f a -> t f a
class Lower c t | t -> c where lower' :: c f => t f a -> f a
class (Lift c (FreeT c)) => Free' c where
  data FreeT c :: (k -> *) -> k -> *

instance Free' Map where newtype FreeT Map f a = M (forall x. (a -> x) -> f x)
instance Lift Map (FreeT Map) where lift' fa = M (`map` fa)
instance Lower Trivial (FreeT Map) where lower' (M k) = k (\a -> a)
-}

  
  

instance CoFree Monad where
  type CoFree'C Monad = Pure
  newtype CoT Monad f a = Bind {runBind :: forall r. ((a -> f r) -> f r) }
  liftc m = Bind (\f -> bind f m)
  lowerc (Bind k) = k pure



{-data instance Free Monad f a = Return a | More (Free Monad f a)-}

{-data instance Free Applicative f a where-}
  {-Pure :: a -> Free Applicative f a-}
  {-Ap :: f a -> Free Applicative f (a -> b) -> Free Applicative f b-}

instance Free Map where
  newtype T Map f a = Yoneda {runYoneda :: forall x. (a -> x) -> f x}
  lift fa = Yoneda (`map` fa)
  lower (Yoneda k) = k (\a -> a)

{-data instance CoFree Map f a = forall x. CoYoneda (f x) (x -> a)-}
{-liftCoyoneda fa = CoYoneda fa (\a -> a)-}
{-lowerCoyoneda (CoYoneda fx xa) = map xa fx-}
