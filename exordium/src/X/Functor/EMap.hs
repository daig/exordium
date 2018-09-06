module X.Functor.EMap (module X.Functor.EMap, module X) where
import X.Functor.Map as X
import X.Data.E as X
import Data.Bool
import X.Data.Maybe

-- | emap (R . f) = map f
--
--   emap f . emap g = emap (f <=< g)
class Map f => EMap e f where
  {-# minimal emap | throwing #-}
  emap :: (a -> E e b) -> f a -> f b
  emap aeb fa = throwing (map aeb fa)
  throwing :: f (E e a) -> f a
  -- ^ O f (E a) --> f
  throwing = emap (\ea -> ea)
  {-filter :: e ~ () => (a -> Bool) -> f a -> f a-}
  {-filter p = emap (\a -> if p a then R a else L ())-}

instance EMap List_Nil [] where
  emap f = go where
    go = \case
      [] -> []
      a:as -> case f a of {L{} -> []; R b -> b : go as}
data List_Skip = List_Skip
data List_Nil = List_Nil
instance EMap List_Skip [] where
  emap f = go where
    go = \case
      [] -> []
      a:as -> case f a of {L{} -> go as; R b -> b : go as}
instance EMap e (E e) where
  emap f = \case
    L e -> L e
    R a -> f a
data Maybe_Nothing = Maybe_Nothing
instance EMap Maybe_Nothing Maybe where
  emap f = \case
    Nothing -> Nothing
    Just a -> case f a of {L{} -> Nothing; R b -> Just b}
