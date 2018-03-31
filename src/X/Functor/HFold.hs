module X.Functor.HFold (module X.Functor.HFold,module X) where
import X.Functor.Fold as X
import X.Num.Add0 as X
import X.Data.These
import X.Num.Scale as X

class Fold t => HFold e t where
  hfoldMap :: (Scale r m, Add0 m) => (e -> r) -> (a -> m) -> t a -> m

class (Fold0 t, HFold e t) => HFold0 e t where
  hfoldMap0 :: Zero m => (e -> m) -> (a -> m) -> t a -> m
class HFold e t => HFold1 e t where -- Fold1 superclass is weird here
  hfoldMap1 :: Scale r m => (e -> r) -> (a -> m) -> t a -> m
class (Fold_ t, HFold0 e t, HFold1 e t) => HFold_ e t where
  hfoldMap_ :: (e -> m) -> (a -> m) -> t a -> m

{-instance HFold e (These e) where hfoldMap = hfoldMap1-}
{-instance HFold1 e (These e) where-}
  {-hfoldMap1 em am = \case-}
    {-This e -> em e-}
    {-That a -> am a-}
    {-These e a -> em e `add` am a-}
