module X.Functor.Zip.Internal (V2(..),module X) where
import {-# source #-} X.Functor.Zip as X

data V2 a = V2 {v2a :: ~a, v2b :: ~a} 
instance Map V2 where map f (V2 a b) = V2 (f a) (f b)
instance Pure V2 where pure a = V2 a a
instance Zip V2 where distribute fta = V2 (map v2a fta) (map v2b fta)
instance Apply V2 where ap (V2 f g) (V2 a b) = V2 (f a) (g b)
instance Applicative V2
instance One a => One (V2 a) where one = pure one
