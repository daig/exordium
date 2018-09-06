module X.Ops.Num where
import X.Num.Recip
import X.Num.Negate
import X.Num.Diff

(+) :: Add a => a -> a -> a
infixl 6 +
{-# inline (+) #-}
(+) = add

(-) :: Negate a => a -> a -> a
infixl 6 -
{-# inline (-) #-}
(-) = sub

(*) :: Mul a => a -> a -> a
infixl 7 *
{-# inline (*) #-}
(*) = mul


(.-.) :: Diff a x => x -> x -> a
infixl 6 .-.
{-# inline (.-.) #-}
(.-.) = diff

(+.) :: Act a x => a -> x -> x
infixr 5 +.
{-# inline (+.) #-}
(+.) = act


(/) :: Recip a => a -> a -> a
infixl 7 /
{-# inline (/) #-}
(/) = divide


