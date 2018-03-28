module X.Optic.Tuple where
import X.Arrow.Traversed

class Field1 s a b t | s -> a, t -> b, s b -> t, t a -> s where _1 :: Traversed_ p => p a b -> p s t
class Field2 s a b t | s -> a, t -> b, s b -> t, t a -> s where _2 :: Traversed_ p => p a b -> p s t

instance Field1 (a,x) a b (b,x) where _1 = lens (\(a,_) -> a) (\(_,b) a -> (a,b))
instance Field1 (a,x,y) a b (b,x,y) where _1 = lens (\(a,_,_) -> a) (\(_,b,c) a -> (a,b,c))

instance Field2 (x,a) a b (x,b) where _2 = lens (\(_,a) -> a) (\(a,_) b -> (a,b))
instance Field2 (x,a,y) a b (x,b,y) where _2 = lens (\(_,a,_) -> a) (\(a,_,c) b -> (a,b,c))
