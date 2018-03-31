module X.Functor.HMap (HMap(..), module X) where
import X.Functor.Map as X
import X.Data.Maybe
import X.Data.E
import X.Num.Zero

-- | "Heterogenious map" which can impute missing values/recover from errors.
--   dual to IMap
--
--   hmap @e ef f . hmap @e eg g = hmap (f . eg) (f . g)
--   hmap ef f = map f . impute ef
class Map f => HMap e f where
  hmap :: (e -> b) -> (a -> b) -> f a -> f b

  impute :: (e -> a) -> f a -> f a
  impute ea = hmap ea (\a -> a)

  catchE :: f a -> f (E e a)
  catchE = hmap L R

instance Zero e => HMap e Maybe where
  hmap eb ab = \case
    Nothing -> Just (eb zero)
    Just a -> Just (ab a)
instance Zero e => HMap e [] where -- TODO: is this right?
  hmap eb ab = go where
    go = \case
      [] -> zs
      a:as -> ab a : go as
    zs = eb zero : zs
instance HMap e (E e) where
  hmap eb ab = \case
    L e -> R (eb e)
    R a -> R (ab a)
