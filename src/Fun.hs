module Fun
  ((#),(##), (###)
  ,(!),(!!)
  ,($&)
  ,module X) where
import Category as X
import Prelude (seq)

(!) = \a _ -> a
(!!) = seq

f # a = f a
flip, (##) :: (a -> b -> c) -> b -> a -> c
(f ## b) a = f a b
flip = (##)
(f ### c) a b = f a b c
infixl 3 #,##,###

f $& a = f a a

{-(#&) :: (a -> a -> b) -> a -> b-}
{-f #& a = f a a-}

{-(%) :: a -> (a -> b) -> a-}
{-a % f = f a-}
{-infixl 1 %-}

{-(&%) = flip (#&)-}


{-(?),(??) :: Bool -> a -> a -> a-}
{-(b ? f) t = if b then t else f-}
{-(b ?? t) f = if b then t else f-}
{-(???) :: a -> b -> Bool -> a-}
{-(f ??? t) b = if b then t else f-}
{-infix 3 ?, ??-}
