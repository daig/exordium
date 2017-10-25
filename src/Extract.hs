module Extract (Extract(..), module X) where
import Map as X
import Sum as X
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
class Map f => Extract f where
  {-# minimal extract | codistL,codistR #-}
  extract :: f a -> a
  extract f = case codistL (map R f) of {R a -> a; L _ -> error "impossible"}
  codistL :: f (E a b) -> E (f a) b 
  codistL f = case extract f of {L _ -> L $ map (\(L a) -> a) f; R b -> R b}
  codistR :: f (E a b) -> E a (f b)
  codistR f = case extract f of {L b -> L b; R _ -> R $ map (\(R b) -> b) f}
