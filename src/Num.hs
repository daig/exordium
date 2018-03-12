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
import Ord
import Map.Pro
import GHC.Integer
import Optic.Prism (lens0,Traversed0)
import E (E(..))

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
class Add a where
  add :: a -> a -> a
  scale1 :: Natural -> a -> a
  scale1 n = scale1# (n P.+ 1) 
  {-sumWith1 :: FoldMap1 f => (x -> a) -> f x -> a-} -- why is this needed?

class Zero a where zero :: a
class Mul a where
  mul :: a -> a -> a
  -- | Raise to the @n+1@ power
  pow1 :: Natural -> a -> a
  pow1 n = pow1# (n P.+1)

-- | Scale by a non-zero @Natural@, this is not checked and will loop on 0.
scale1# :: Add a => Natural -> a -> a
scale1# = rep1# add

-- | Combine a value with itself n mul for positive n. This is not checked and will loop on 0.
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
pow1# :: Mul a => Natural -> a -> a
pow1# = rep1# mul

class One a where one :: a
-- | one * a = a * one = a
class (Mul m, One m) => MulOne m where
  pow0 :: Natural -> m -> m
  pow0 0 = \_ -> one
  pow0 n = pow1# n

pow0_pow1 :: MulOne a => Natural -> a -> a
pow0_pow1 n = pow0 (n P.+ 1)
-- | zero + a = a + zero = a
class (Add a, Zero a) => AddZero a where
  scale0 :: Natural -> a -> a
  scale0 0 = \_ -> zero
  scale0 n = scale1# n

-- a - a = zero
-- (a - b) - c = a - (b + c)
class AddZero a => Minus a where
  {-# minimal minus | negate #-}
  minus :: a -> a -> a
  a `minus` b = a `add` negate b
  negate :: a -> a
  negate = minus zero
  scalei :: Integer -> a -> a
  scalei n a = case compare n 0 of
    EQ -> zero
    LT -> scale1# (P.fromInteger (P.abs n)) (negate a)
    GT -> scale1# (P.fromInteger n) a


-- | offset (add m n) s = offset m (offset n s)
-- | offset zero s = s
class AddZero m => Offset m s where offset :: m -> s -> s

-- | diff a b `offset` a = b.
--
--  implied: (diff b c `add` diff a b) * s = diff b c * diff a b * a = c
--
--   diff a a = zero -- TODO: is this implied by above?
class Offset m s => Diff m s where diff :: s -> s -> m

-- | (m*n) `scale` s = m `scale` n `scale` s
-- | (m+n) `scale` s = scale m s `add` scale n s
class Mul m => Scale m s | s -> m where scale :: m -> s -> s

-- | pow m (pow n s) = pow (mul m n) s
--   pow (add m n) = pow m s `mul` pow n s
class (AddMul m, Mul s) => Pow m s | s -> m where pow :: m -> s -> s

class Zero a => Zero' a where zero' :: a -> Bool

pattern Zero :: Zero' a => a
pattern Zero <- (zero' -> T) where Zero = zero



class MulOne m => Recip m where
  {-# minimal recip | divide #-}
  recip :: m -> m
  recip = divide one
  divide :: m -> m -> m
  divide m n = m `mul` recip n
  powi :: Integer -> m -> m
  powi n m = case P.compare n 0 of
    EQ -> one 
    LT -> pow1# (P.fromInteger (P.abs n)) (recip m)
    GT -> pow1# (P.fromInteger n) m


{-class (Offset m s, Add m) => Quotient m s where quot :: s -> s -> (m,s)-}


-- | Near Semiring
-- a(b + c) = ab + ac
-- (a + b)c = ac + bc
class (Add m, Mul m) => AddMul m


-- | s + s = s
class Add s => IdempotentAdd s

-- | s * s = s
class Mul s => IdempotentMul s

-- | a + b = b + a
class Add s => CommuteAdd s

-- | a * b = b * a
class Mul s => CommuteMul s


-- Instances --
instance Zero Natural where zero = 0
instance One Natural where one = 1
instance Add Natural where add = (P.+)
instance AddZero Natural
instance Mul Natural where mul = (P.*)
instance MulOne Natural
instance AddMul Natural

{-instance Add (a -> a) where f `add` g = \a -> f (g a)-}
{-instance Scale Natural (a -> a) where-}
  {-scale 0 _ = \a -> a-}
  {-scale n f = \a -> scale (n `minus` 1) f (f a)-}
{-instance (Add a, Add b) => Add (a,b) where (a,b) `add` (x,y) = (add a x,add b y)-}
{-instance Add Int where add = (P.+)-}
{-instance Scale Natural Int where-}
  {-scale n m = P.fromIntegral n `add` m-}

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

{-instance Mul Int where mul = (P.*)-}

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
instance Add (a -> a) where add f g = \x -> f (g x)
instance AddZero (a -> a)

{-instance Add b => Mul (a -> b) where mul f g = \x -> f x `add` g x-}
{-instance Zero b => One (a -> b) where one = \_ -> zero-}
{-instance AddZero b => MulOne (a -> b)-}
{-instance (Add a) => AddMul (a -> a)-}
{-instance Scale (a -> b -> b) (a -> b) where scale f g = \a -> f a (g a)-}
ff f g a = f a `mul` g a
xx x as = add 10 x : as
yy y as = mul 2 y : as
hh = (:[])
{-instance Add a => Pow (a -> a -> a) (a -> a) where pow f g = \x -> f x (g x)-}
{-instance AddZero (a -> a)-}
{-instance (AddZero a, AddZero b) => AddZero (a,b)-}
{-instance AddZero Int-}

{-class FMin f where fmin :: (m -> s -> s) -> f m -> f s -> f s-}
{-class FMin f => FTop f where ftop :: a -> f a-}

{-class FAdd f where fadd :: f a -> f a -> f a-}
{-class FAdd f => FEmpty f where fempty :: f a-}

{-class FMax f where fmax :: s -> (m -> s -> s) -> f m -> f s -> f s-}
{-class FMax f => FBottom f where fbottom :: f a-}

{-class FMul f where fmul :: (m -> s -> s) -> f m -> f s -> f s-}
{-class FMul f => Pure f where pure :: a -> f a-}

{-instance Offset (a -> a) a where offset f a = f a-}
{-instance FMin ((->) a) where fmin op am as = \a -> am a `op` as a-}
{-instance FMax ((->) a) where fmin op am as = \a -> am a `op` as a-}
{-instance FScale ((->) a) where fscale am as = \a -> am a `scale` as a-}



-- | Left Near Semiring.
--   a(b + c) = ab + ac
{-class (Add m, Mul m) => LeftAddMul m-}

-- | Right Near Semiring.
-- (a + b)c = ac + b
{-class (Add m, Mul m) => RightAddMul m-}

instance Enum Natural
instance Succ Natural where succ = P.succ
instance Pred Natural where pred = P.pred

instance Zero' Natural where zero' = (P.== 0)
