{-# language MagicHash #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Num where
import Num.Zero'
import Num.Sub
import Num.One'
import Num.Recip
import Num.Scale
import Num.Diff
import Num.FromInteger
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



{-class (Offset m s, Add m) => Quotient m s where quot :: s -> s -> (m,s)-}


-- Instances --

{-instance Add (a -> a) where f `add` g = \a -> f (g a)-}

{-instance Zero (a -> a) where zero = \a -> a-}
{-instance (Zero a, Zero b) => Zero (a,b) where zero = (zero,zero)-}


{-instance One a => One (x -> a) where one = \_ -> one-}
{-instance One a => One [a] where one = [one]-}
{-instance One a => One (I a) where one = I one-}
--instance Zero a => One (K a x)

{-instance Zero (Maybe a) where zero = Nothing-}
{-instance Zero [a] where zero = []-}

{-instance Min (a -> b) -- TODO: use unamb-}
{-instance Max (a -> b) -- TODO-}
{-instance Top (a -> b) where  -- Never halt-}
{-instance Bottom (a -> b) where -- Error immediately-}
instance Zero (a -> a) where zero = \a -> a
instance Add (a -> a) where add f g = \x -> f (g x)
{-instance Scale Natural (a -> a) where scale = scale0-}

{-instance Add b => Mul (a -> b) where mul f g = \x -> f x `add` g x-}
{-instance Zero b => One (a -> b) where one = \_ -> zero-}
{-instance Zero b => One (a -> b)-}
{-instance (Add a) => Rg (a -> a)-}
{-instance Scale (a -> b -> b) (a -> b) where scale f g = \a -> f a (g a)-}
{-ff f g a = f a `mul` g a-}
{-xx x as = add 10 x : as-}
{-yy y as = mul 2 y : as-}
{-hh = (:[])-}
{-instance Add a => Pow (a -> a -> a) (a -> a) where pow f g = \x -> f x (g x)-}
{-instance Zero (a -> a)-}
{-instance (Zero a, Zero b) => Zero (a,b)-}
{-instance Zero Int-}

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




