{-# language MagicHash #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Num where
import qualified Prelude as P
import Int (Int)
import Maybe (Maybe(..))
import I (I(..))
{-import FoldMap -}
import GHC.Natural
import Map.Pro

import Word


class Succ a where
  succ :: a -> a
  offsetp :: Natural -> a -> a
  offsetp 0 a = a
  offsetp n a = succ (offsetp (pred n) a)
class Pred a where
  pred :: a -> a
  offsetn :: Natural -> a -> a
  offsetn 0 a = a
  offsetn n a = pred (offsetn (pred n) a)
-- | succ < pred = pred < succ = id
class (Succ a, Pred a) => Enum a where
  offset' :: P.Integer -> a -> a
  offset' n | n P.< 0 = offsetn (P.fromIntegral (P.abs n))
  offset' n = offsetp (P.fromIntegral n)
  
-- | a + (b + c) = (a + b) + c
class Plus a where
  plus :: a -> a -> a
  scale1 :: Natural -> a -> a
  scale1 n = scale1# (n P.+ 1) 
  {-sumWith1 :: FoldMap1 f => (x -> a) -> f x -> a-} -- why is this needed?

class Zero a where zero :: a
class Times a where
  times :: a -> a -> a
  -- | Raise to the @n+1@ power
  pow1 :: Natural -> a -> a
  pow1 n = pow1# (n P.+1)

-- | Scale by a non-zero @Natural@, this is not checked and will loop on 0.
scale1# :: Plus a => Natural -> a -> a
scale1# = rep1# plus

-- | Combine a value with itself n times for positive n. This is not checked and will loop on 0.
rep1# :: (a -> a -> a) -> Natural -> a -> a
rep1# op y0 x0 = f x0 y0 where
  f x y 
    | P.even y = f (x `op` x) (y `P.quot` 2)
    | y P.== 1 = x
    | P.otherwise = g (x `op` x) ((y P.- 1) `P.quot` 2) x
  g x y z 
    | P.even y = g (x `op` x) (y `P.quot` 2) z
    | y P.== 1 = x `op` z
    | P.otherwise = g (x `op` x) ((y P.- 1) `P.quot` 2) (x `op` z)

-- | Raise to a non-zero @Natural@ power, this is not checked and will loop on 0.
pow1# :: Times a => Natural -> a -> a
pow1# = rep1# times

class One a where one :: a
-- | one * a = a * one = a
class (Times m, One m) => TimesOne m where
  pow0 :: Natural -> m -> m
  pow0 0 = \_ -> one
  pow0 n = pow1# n

pow0_pow1 :: TimesOne a => Natural -> a -> a
pow0_pow1 n = pow0 (n P.+ 1)
-- | zero + a = a + zero = a
class (Plus a, Zero a) => PlusZero a where
  scale0 :: Natural -> a -> a
  scale0 0 = \_ -> zero
  scale0 n = scale1# n

-- a - a = zero
-- (a - b) - c = a - (b + c)
class PlusZero a => Minus a where
  {-# minimal minus | negate #-}
  minus :: a -> a -> a
  a `minus` b = a `plus` negate b
  negate :: a -> a
  negate = minus zero


-- | offset (plus m n) s = offset m (offset n s)
-- | offset zero s = s
class PlusZero m => Offset m s where offset :: m -> s -> s

-- | diff a b `offset` a = b.
--
--  implied: (diff b c `plus` diff a b) * s = diff b c * diff a b * a = c
--
--   diff a a = zero -- TODO: is this implied by above?
class Offset m s => Diff m s where diff :: s -> s -> m

-- | (m*n) `scale` s = m `scale` n `scale` s
-- | (m+n) `scale` s = scale m s `plus` scale n s
class Times m => Scale m s | s -> m where scale :: m -> s -> s

-- | pow m (pow n s) = pow (times m n) s
--   pow (plus m n) = pow m s `times` pow n s
class (PlusTimes m, Times s) => Pow m s | s -> m where pow :: m -> s -> s


{-class (Offset m s, Plus m) => Quotient m s where quot :: s -> s -> (m,s)-}


-- | Near Semiring
-- a(b + c) = ab + ac
-- (a + b)c = ac + bc
class (Plus m, Times m) => PlusTimes m


-- | s + s = s
class Plus s => Idempotent s

-- | a + b = b + a
class Plus s => Abelian s


-- Instances --
instance Zero Natural where zero = 0
instance One Natural where one = 1
instance Plus Natural where plus = (P.+)
instance PlusZero Natural
instance Times Natural where times = (P.*)
instance TimesOne Natural
instance PlusTimes Natural

{-instance Plus (a -> a) where f `plus` g = \a -> f (g a)-}
{-instance Scale Natural (a -> a) where-}
  {-scale 0 _ = \a -> a-}
  {-scale n f = \a -> scale (n `minus` 1) f (f a)-}
{-instance (Plus a, Plus b) => Plus (a,b) where (a,b) `plus` (x,y) = (plus a x,plus b y)-}
{-instance Plus Int where plus = (P.+)-}
{-instance Scale Natural Int where-}
  {-scale n m = P.fromIntegral n `plus` m-}

{-instance Zero (a -> a) where zero = \a -> a-}
{-instance (Zero a, Zero b) => Zero (a,b) where zero = (zero,zero)-}
{-instance Zero Int where zero = 0-}
{-instance Zero Integer where zero = 0-}
instance Zero Word   where zero = 0
instance Zero Word8  where zero = 0
instance Zero Word16 where zero = 0
instance Zero Word32 where zero = 0
instance Zero Word64 where zero = 0
{-instance Zero Bool where zero = False-}

{-instance Times Int where times = (P.*)-}

{-instance One a => One (x -> a) where one = \_ -> one-}
{-instance One () where one = ()-}
{-instance One a => One [a] where one = [one]-}
{-instance One a => One (I a) where one = I one-}
--instance Zero a => One (K a x)

{-instance Zero (Maybe a) where zero = Nothing-}
{-instance Zero [a] where zero = []-}
{-instance Zero () where zero = ()-}

{-instance Min (a -> b) -- TODO: use unamb-}
{-instance Max (a -> b) -- TODO-}
{-instance Top (a -> b) where  -- Never halt-}
{-instance Bottom (a -> b) where -- Error immediately-}
instance Zero (a -> a) where zero = \a -> a
instance Plus (a -> a) where plus f g = \x -> f (g x)
instance PlusZero (a -> a)

{-instance Plus b => Times (a -> b) where times f g = \x -> f x `plus` g x-}
{-instance Zero b => One (a -> b) where one = \_ -> zero-}
{-instance PlusZero b => TimesOne (a -> b)-}
{-instance (Plus a) => PlusTimes (a -> a)-}
{-instance Scale (a -> b -> b) (a -> b) where scale f g = \a -> f a (g a)-}
ff f g a = f a `times` g a
xx x as = plus 10 x : as
yy y as = times 2 y : as
hh = (:[])
{-instance Plus a => Pow (a -> a -> a) (a -> a) where pow f g = \x -> f x (g x)-}
{-instance PlusZero (a -> a)-}
{-instance (PlusZero a, PlusZero b) => PlusZero (a,b)-}
{-instance PlusZero Int-}

class FMin f where fmin :: (m -> s -> s) -> f m -> f s -> f s
class FMin f => FTop f where ftop :: a -> f a

class FPlus f where fplus :: f a -> f a -> f a
class FPlus f => FEmpty f where fempty :: f a

class FMax f where fmax :: s -> (m -> s -> s) -> f m -> f s -> f s
class FMax f => FBottom f where fbottom :: f a

class FTimes f where ftimes :: (m -> s -> s) -> f m -> f s -> f s
class FTimes f => Pure f where pure :: a -> f a

instance Offset (a -> a) a where offset f a = f a
instance FMin ((->) a) where fmin op am as = \a -> am a `op` as a
instance FMax ((->) a) where fmin op am as = \a -> am a `op` as a
instance FScale ((->) a) where fscale am as = \a -> am a `scale` as a



-- | Left Near Semiring.
--   a(b + c) = ab + ac
{-class (Plus m, Times m) => LeftPlusTimes m-}

-- | Right Near Semiring.
-- (a + b)c = ac + b
{-class (Plus m, Times m) => RightPlusTimes m-}

instance Enum Natural
instance Succ Natural where succ = P.succ
instance Pred Natural where pred = P.pred
