module Fun where

f # a = f a
flip, (##) :: (a -> b -> c) -> b -> a -> c
(f ## b) a = f a b
flip = (##)
(f ### c) a b = f a b c
infixl 3 #,##,###

(#&) :: (a -> a -> b) -> a -> b
f #& a = f a a

(%) :: a -> (a -> b) -> a
a % f = f a
infixl 1 %

(&%) = flip (#&)


(!) :: a -> b -> a
a ! b = a
(!!) :: a -> b -> b

(?),(??) :: Bool -> a -> a -> a
(b ? f) t = if b then t else f
(b ?? t) f = if b then t else f
(???) :: a -> b -> Bool -> a
(f ??? t) b = if b then t else f
infix 3 ?, ??

{-(<) :: (x -> b) -> (a -> x) -> a -> b-}
{-f < g = \a -> f (g a)-}
{-infixr 9 <-}

{-(>) ::  (a -> x) -> (x -> b) -> a -> b-}
{-f > g = \a -> g (f a)-}
{-infixr 9 >-}
