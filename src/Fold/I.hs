module Fold.I (module Fold.I, module X) where
import Num.Add0 as X
import Prelude (Enum(..))

class IFold i t where
  {-# minimal ifoldMap | ifoldr #-}
  ifoldMap :: Add0 m => (i -> a -> m) -> t a -> m
  ifoldMap f t = ifoldr (\i a m -> f i a `add` m) zero t -- TODO: check the order
  ifoldr :: (i -> a -> b -> b) -> b -> t a -> b
  {-ifoldr c z t = ifoldMap c t z-} -- TODO: need an Add instances for (->)
  {-foldl :: (b -> a -> b) -> b -> t a -> b-}


class IFold i t => IFold0 i t where
  ifoldMap0 :: Zero m => (i -> a -> m) -> t a -> m

class IFold i t => IFold1 i t where
  ifoldMap1 :: Add s => (i -> a -> s) -> t a -> s


instance Zero i => IFold0 i ((,) x) where ifoldMap0 f (_,x) = f zero x
instance Zero i => IFold1 i ((,) x) where ifoldMap1 f (_,y) = f zero y
instance Zero i => IFold i ((,) x) where ifoldMap f (_,a) = f zero a
instance Enum i => IFold i [] where
  ifoldMap = go' where
    go' f = go (toEnum 0) where
      go i = \case
	[] -> zero
	a:as -> f i a `add` go (succ i) as
