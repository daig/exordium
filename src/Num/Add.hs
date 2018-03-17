{-# language MagicHash #-}
module Num.Add (Add(..), scale1#,module X) where
import GHC.Natural as X
import GHC.Integer
import Bool
import Type.Int
import Type.Word
import qualified Prelude as P

-- | a + (b + c) = (a + b) + c
class Add a where
  add :: a -> a -> a
  scale1 :: Natural -> a -> a
  scale1 n = scale1# (n P.+ 1) 
  {-sumWith1 :: Fold1 f => (x -> a) -> f x -> a-} -- why is this needed?

-- | Scale by a non-zero @Natural@, this is not checked and will loop on 0.
scale1# :: Add a => Natural -> a -> a
scale1# y0 x0 = f x0 y0 where
  f x y 
    | P.even y = f (x `add` x) (y `P.quot` 2)
    | y P.== 1 = x
    | P.otherwise = g (x `add` x) ((y P.- 1) `P.quot` 2) x
  g x y z 
    | P.even y = g (x `add` x) (y `P.quot` 2) z
    | y P.== 1 = x `add` z
    | P.otherwise = g (x `add` x) ((y P.- 1) `P.quot` 2) (x `add` z)

instance Add Natural where add = (P.+)
instance (Add a, Add b) => Add (a,b) where (a,b) `add` (x,y) = (add a x,add b y)
instance Add Int where add = (P.+)
instance Add Word where add = (P.+)
instance Add Integer where add = (P.+)
instance Add Bool where add = (P./=)
instance Add () where add _ _ = ()
