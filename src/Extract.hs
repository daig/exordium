module Extract (TraverseE(..), module X) where
import Map as X
import Sum as X
import LinFoldMap as X
import Fun
import Prelude (error)

-- | Natural laws:
-- codistR < map (left f) = left f
-- codistR < map (right f) = right (map f)
--
-- Derived laws:
-- extract < map f = f
-- codistL < map L = L
-- codistL < map R = R < extract
-- codistR < map R = R
-- codistR < map L = L < extract
class (LinFoldMap f, Map f) => TraverseE f where
  traverseL :: (a -> E a x) -> f a -> E (f a) x
  traverseL f = sequenceL < map f
  traverseR :: (a -> E x a) -> f a -> E x (f a)
  traverseR f = sequenceR < map f
  sequenceL :: f (E a b) -> E (f a) b 
  sequenceL f = case fold_ f of {L _ -> L $ map (\(L a) -> a) f; R b -> R b}
  sequenceR :: f (E a b) -> E a (f b)
  sequenceR f = case fold_ f of {L b -> L b; R _ -> R $ map (\(R b) -> b) f}

{-traverseFst :: (a -> (a,x)) -> f a -> (f a, x)-}
{-sequenceFst :: f (a,b) -> (f a, b)-}

{-fold_Default f = case sequenceL (map R f) of {R a -> a; L _ -> error "impossible"}-}
fold_Default f = traverseL R > \case {R a -> a}
