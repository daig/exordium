module X.Functor.IFold (module X.Functor.IFold, module X) where
import X.Num.Add0 as X
import X.Num.FromNatural
import X.Type.I

class IFold i t where
--  {-# minimal ifoldMap | ifoldr #-}
  ifoldMap :: Add0 m => (i -> a -> m) -> t a -> m
  {-ifoldMap f t = ifoldr (\i a m -> f i a `add` m) zero t -- TODO: check the order-}
  {-ifoldr :: (i -> a -> b -> b) -> b -> t a -> b-}
  {-ifoldr c z t = ifoldMap c t z-} -- TODO: need an Add instances for (->)
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}


class IFold i t => IFold0 i t where
  ifoldMap0 :: Zero m => (i -> a -> m) -> t a -> m

class IFold i t => IFold1 i t where
  ifoldMap1 :: Add s => (i -> a -> s) -> t a -> s

class (IFold0 i t,IFold1 i t) => IFold_ i t where
  ifoldMap_ :: (i -> a -> s) -> t a -> s


instance Zero i => IFold0 i ((,) x) where ifoldMap0 = ifoldMap_
instance Zero i => IFold1 i ((,) x) where ifoldMap1 = ifoldMap_
instance Zero i => IFold_ i ((,) x) where ifoldMap_ f (_,y) = f zero y
instance Zero i => IFold i ((,) x) where ifoldMap f (_,a) = f zero a
instance FromNatural i => IFold i [] where
  ifoldMap = go' where
    go' f = go zero where
      go i = \case
        [] -> zero
        a:as -> f i a `add` go (add one i) as
instance Zero i => IFold i I where ifoldMap = ifoldMap_
instance Zero i => IFold0 i I where ifoldMap0 = ifoldMap_
instance Zero i => IFold1 i I where ifoldMap1 = ifoldMap_
instance Zero i => IFold_ i I where ifoldMap_ iab (I a) = iab zero a
