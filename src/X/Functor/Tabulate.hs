module X.Functor.Tabulate
  (module X.Functor.Tabulate
  ,module X) where
import X.Arrow.Promap as X
import X.Functor.Zip as X
{-import X.Optic.Review-}
{-import X.Optic.View-}
import X.Kind.Type
import X.Type.I

class Zip f => Tabulate f where
--  {-# minimal indexed | index,tabulate #-}
  type Ix f :: Type
  {-indexed :: f ~~= ((->) (Ix f))-}
  {-indexed = isoF index tabulate-}
  index :: f a -> Ix f -> a
  {-index = view indexed-}
  tabulate :: (Ix f -> a) -> f a
  {-tabulate = _Review indexed-}

tabulate_map :: Tabulate f => (a -> b) -> f a -> f b
tabulate_map f fa = tabulate (\i -> f (index fa i))

tabulate_distribute :: (Tabulate i, Map f) => f (i a) -> i (f a)
tabulate_distribute fi = tabulate (\k -> map (`index` k) fi)

tabulate_ap :: Tabulate f => f (a -> b) -> f a -> f b
tabulate_ap f g = tabulate (index f `ap` index g)

instance Tabulate ((->) x) where
  type Ix ((->) x) = x
  index f = f
  tabulate f = f
instance Tabulate I where
  type Ix I = ()
  index (I a) = \_ -> a
  tabulate f = I (f ())
