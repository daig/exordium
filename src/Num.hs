{-# language MagicHash #-}
{-# language TypeInType #-}
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
import Prim.Word
import Kind.Type

data family B (t :: TYPE k) :: *
data instance B Word# = Word Word#
data instance B Int# = Int Int#
data instance B Float# = Float Float#
data instance B Double# = Double Double#



-- | a + (b + c) = (a + b) + c
class Add a where
  add :: a -> a -> a
  scale1 :: Natural -> a -> a
  scale1 n = scale1# (n P.+ 1) 
  {-sumWith1 :: FoldMap1 f => (x -> a) -> f x -> a-} -- why is this needed?

class Zero a where zero :: a
class Pow Natural1 m => Mul m where mul :: m -> m -> m
  -- Raise to the @n+1@ power
  {-pow1 :: Natural -> a -> a-}
  {-pow1 n = pow1# (n P.+1)-}

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
class (Pow Natural m, Mul m, One m) => MulOne m
  {-pow0 :: Natural -> m -> m-}
  {-pow0 0 = \_ -> one-}
  {-pow0 n = pow1# n-}

{-pow0_pow1 :: MulOne a => Natural -> a -> a-}
{-pow0_pow1 n = pow0 (n P.+ 1)-}
-- | zero + a = a + zero = a
class (Module Natural a, Add a, Zero a) => AddZero a where

scale0 :: (Add a, Zero a) => Natural -> a -> a
scale0 0 = \_ -> zero
scale0 n = scale1# n

--  | act (diff x y) x = y
class Act a x => Diff a x where diff :: x -> x -> a
-- a - a = zero
-- (a - b) - c = a - (b + c)
class (Module Integer s, Diff s s, AddZero s) => Sub s where
  {-# minimal sub | negate #-}
  sub :: s -> s -> s
  a `sub` b = a `add` negate b
  negate :: s -> s
  negate = sub zero
  {-scalei :: Integer -> a -> a-}
  {-scalei n a = case compare n 0 of-}
    {-EQ -> zero-}
    {-LT -> scale1# (P.fromInteger (P.abs n)) (negate a)-}
    {-GT -> scale1# (P.fromInteger n) a-}


-- | diff a b `offset` a = b.
--
--  implied: (diff b c `add` diff a b) * s = diff b c * diff a b * a = c
--


class Zero a => Zero' a where zero' :: a -> Bool

pattern Zero :: Zero' a => a
pattern Zero <- (zero' -> T) where Zero = zero



class (Pow Integer m, MulOne m) => Recip m where
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

-- | A (Left) module over multiplication
-- | r(x*y) = rx * ry
--   (r+s)x = rx * sx
--   (r*s)x = r(sx)
class (Rg r, Mul m) => Pow r m where pow :: r -> m -> m


class (Rg r, Sub r) => Rng r

-- | Near Semiring. Ie a "Ring" without the Identity and Negation
-- a(b + c) = ab + ac
-- (a + b)c = ac + bc
-- so that: (x+y)(s+t) := xs + ys + xt + yt = xs + xt + ys + yt
class (Module r r, Add r, Mul r) => Rg r
-- | (Left) Module:
-- `
--   r(x+y) = rx + ry
--
--  (r + s)x = rx + sx
--
--  (r*s)x = r(sx).
class (Rg r, Add a) => LModule r a where
  scalel :: r -> a -> a
  default scalel :: Module r a => r -> a -> a
  scalel = scale

-- | (Right) Module:
-- `
--   (x+y)r = xr + yr
--
--  x(r + s) = xr + xs
--
--  x(r*s) = (xr)s
class (Rg r, Add a) => RModule r a where
  scaler :: a -> r -> a
  default scaler :: Module r a => a -> r -> a
  scaler a = (`scale` a)


-- | r(as) = (ra)s
class (RModule r a, LModule r a) => Module r a where scale :: r -> a -> a

-- | (a+b)s = a(bs)
class Add a => OffsetL a s where offsetl :: a -> s -> s
-- | s(a+b) = (sa)b
class Add a => OffsetR a s where offsetr :: s -> a -> s

-- | A semigroup action: (m+n)s = m(ns)
--  Acts like scalar offset
class Add a => Act a s where act :: a -> s -> s


-- Instances --
instance Zero Natural where zero = 0
instance One Natural where one = 1
instance Add Natural where add = (P.+)
instance AddZero Natural
instance Mul Natural where mul = (P.*)
instance MulOne Natural
instance Rg Natural
instance LModule Natural Natural where scalel = (P.*)
instance RModule Natural Natural where scaler = (P.*)
instance Module Natural Natural

{-instance Add (a -> a) where f `add` g = \a -> f (g a)-}
{-instance Scale Natural (a -> a) where-}
  {-scale 0 _ = \a -> a-}
  {-scale n f = \a -> scale (n `sub` 1) f (f a)-}
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
instance Module Natural (a -> a) where scale = scale0
instance LModule Natural (a -> a)
instance RModule Natural (a -> a)

{-instance Add b => Mul (a -> b) where mul f g = \x -> f x `add` g x-}
{-instance Zero b => One (a -> b) where one = \_ -> zero-}
{-instance AddZero b => MulOne (a -> b)-}
{-instance (Add a) => Rg (a -> a)-}
{-instance Scale (a -> b -> b) (a -> b) where scale f g = \a -> f a (g a)-}
{-ff f g a = f a `mul` g a-}
{-xx x as = add 10 x : as-}
{-yy y as = mul 2 y : as-}
{-hh = (:[])-}
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

-- | Positive naturals
newtype Natural1 = Natural1# Natural
  deriving newtype (Pow Natural,Pow Natural1, Rg, Module Natural1, LModule Natural1, RModule Natural1, Mul ,Add,P.Show)
-- | Construct a positive natural from a regular natural offset by 1
pattern Natural1 :: Natural -> Natural1
pattern Natural1 n <- (Natural1# ((\x -> x P.- 1) -> n)) where
  Natural1 n = Natural1# (add 1 n)
instance Pow Natural1 Natural where pow (Natural1# n) = pow n
instance RModule Natural1 Natural
instance LModule Natural1 Natural 
instance Module Natural1 Natural where scale (Natural1# n) = scalel n
instance Pow Natural Natural where pow n m = m P.^ n




instance Zero' Natural where zero' = (P.== 0)
