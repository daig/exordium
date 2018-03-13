module Bits where
import Num
import Mapped
import Bool
import Optic.Grate
import Optic.View
import Optic.Iso
import Word
import qualified Data.Bits  as P
import Cons

class Bits s where
  _Bits :: Mapped p => p Bool Bool -> p s s
  _Bit :: (Distribute f, Promap p) => p (f Bool) (f Bool) -> p s s
  {-toList :: s -> [Bool]-}
  {-toList = view_ _Bits-}
  {-fromList :: [Bool] -> s-}
  zipAnd :: s -> s -> s
  zipOr :: s -> s -> s

instance Bits [Bool] where
  _Bits = zipped

instance Distribute [] where
  distribute x = [map f x,map ff x, map fff x] where
    f [a,_,_] = a
    ff [_,b,_] = b
    fff [_,_,c] = c


tt = [T,T,F]
ttt = [F,T,T]

{-instance Bits Word Word where _Bits = map (P.testBit w)-}


class Traversed' p => Build p where
  {-build :: FoldMap f => (s -> ) -> (forall f. FoldMap f => f b -> t)-}
