module X.Data.Exist.HList (module X.Data.Exist.HList,module X) where
import X.Arrow.Promap as X
import X.Kind.Type

data HList (xs :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList xs -> HList (a ': xs)

_HCons :: Promap p => p (a,HList ts) (b, HList ts) -> p (HList (a ': ts)) (HList (b ': ts))
_HCons = promap pat (\(a,xs) -> HCons a xs) where
  pat :: HList (a ': ts) -> (a,HList ts)
  pat (HCons a xs) = (a,xs)
