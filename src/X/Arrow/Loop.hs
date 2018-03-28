module X.Arrow.Loop (module X.Arrow.Loop, module X) where
import X.Arrow.Promap as X
import X.Data.E as X
import Prelude ((.))

class Promap p => Loop' p where
  {-# minimal loopLeft | loopRight #-}
  loopLeft :: p (E a y) (E b y) -> p a b
  loopLeft p = loopRight (promap swap swap p)
  loopRight :: p (E x a) (E x b) -> p a b
  loopRight p = loopLeft (promap swap swap p)

class Promap p => Loop_ p where
  {-# minimal loopFirst | loopSecond #-}
  loopFirst :: p (a, y) (b, y) -> p a b
  loopFirst p = loopSecond (promap swap swap p)
  loopSecond :: p (x, a) (x, b) -> p a b
  loopSecond p = loopFirst (promap swap swap p)

instance Loop' (->) where
  loopLeft f = go . L where go = bifoldMap_ (\x -> x) (go . R) . f
  loopRight f = go . R where go = bifoldMap_ (go . L) (\x -> x) . f
