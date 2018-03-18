module Bits where
import Num
import Mapped
import Bool
import Optic.Grate
import Optic.View
import Optic.Re
import Optic.Iso
import Type.Word
import qualified Data.Bits  as P
import Mono.Cons

class Mapping s a b t => Zip s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _zip :: Closed p => p a b -> p s t

class Mapping s a b t | s -> a, t -> b, s b -> t, t a -> s where
  _map :: Mapped p => p a b -> p s t

instance Map f => Mapping (f a) a b (f b) where _map = mapped

{-class Bits s where-}
  {-_Bits :: Mapped p => p Bool Bool -> p s s-}
  {-_Bit :: (Distribute f, Promap p) => p (f Bool) (f Bool) -> p s s-}
  {-{-toList :: s -> [Bool]-}-}
  {-{-toList = view_ _Bits-}-}
  {-{-fromList :: [Bool] -> s-}-}
  {-zipAnd :: s -> s -> s-}
  {-zipOr :: s -> s -> s-}

{-instance Zip [a] a b [b] where _zip = zipped-}
instance Distribute f => Zip (f a) a b (f b) where _zip = zipped
{-instance Bits [Bool] where-}
  {-_Bits = zipped-}

instance Distribute [] where
  distribute x = [map f x,map ff x, map fff x] where
    f [a,_,_] = a
    ff [_,b,_] = b
    fff [_,_,c] = c


tt = [T,T,F]
ttt = [F,T,T]

{-{-instance Bits Word Word where _Bits = map (P.testBit w)-}-}


{-class Traversed' p => Build p where-}
  {-{-build :: Fold f => (s -> ) -> (forall f. Fold f => f b -> t)-}-}
