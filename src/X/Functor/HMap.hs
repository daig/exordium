module X.Functor.HMap (module X.Functor.HMap, module X) where
import X.Functor.EMap as X
import X.Num.Zero
import X.Data.Maybe

-- | "Heterogenious map" which can impute missing values/recover from errors.
--   dual to IMap
--
--   hmap @e ef f . hmap @e eg g = hmap (f . eg) (f . g)
--   hmap ef f = map f . impute ef
--   throwing . catching = id
class EMap e f => HMap e f where
  hmap :: (e -> b) -> (a -> b) -> f a -> f b

  impute :: (e -> a) -> f a -> f a
  impute ea = hmap ea (\a -> a)

  catching :: f a -> f (E e a)
  catching = hmap L R

instance HMap Maybe_Nothing Maybe where
  hmap eb ab = \case
    Nothing -> Just (eb Maybe_Nothing)
    Just a -> Just (ab a)

instance HMap List_Nil [] where -- TODO: is this right?
  hmap eb ab = go where
    go = \case
      [] -> zs
      a:as -> ab a : go as
    zs = eb List_Nil : zs
instance HMap e (E e) where
  hmap eb ab = \case
    L e -> R (eb e)
    R a -> R (ab a)
