module X.Functor.Zip.Internal where
import X.Num.One
import X.Functor.Traverse

data V2 a = V2 {v2a :: ~a, v2b :: ~a} 
instance Map V2 where map f (V2 a b) = V2 (f a) (f b)
instance Pure V2 where pure a = V2 a a
instance Apply V2 where ap (V2 f g) (V2 a b) = V2 (f a) (g b)
instance Applicative V2
instance One a => One (V2 a) where one = pure one
instance Fold V2 where foldMap = traverse_foldMap
instance Fold1 V2 where foldMap1 = traverse1_foldMap1
instance Traverse V2 where traverse = traverse1
instance Traverse1 V2 where traverse1 afb (V2 a b) = V2 `map` afb a `ap` afb b
