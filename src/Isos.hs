module Isos
  (module Isos
  ,module X) where
import Class.Dimap as X
import AnIso as X (AnIso)
import AFold as X -- TODO: move to Folds.hs
import Prisms as X
import AnIso
import Type.Re

re :: (Re p s t s t -> Re p s t a b) -> p b a -> p t s
re l = runRe (l (Re (\p -> p)))

type (s ~=. a) b t = AnIso a b a b -> AnIso a b s t
withIso :: (s ~=. a) b t -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (AnIso (\x -> x) (\x -> x)) of {AnIso sa bt -> k sa bt}
under :: (s ~=. a) b t -> (t -> s) -> b -> a
under k = withIso k (\sa bt ts x -> sa (ts (bt x)))
