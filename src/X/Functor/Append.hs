module X.Functor.Append where
import X.Functor.Map as X
import X.Data.E as X
import X.Data.X as X

-- | Associative: fadd (fadd a b) c = fadd a (fadd b c)
class Map f => Append f where
  {-# minimal append | fplus #-}
  append :: f a -> f a -> f a
  append fa fa' = map (\case {L a -> a; R a -> a}) (fplus fa fa')
  fplus :: f a -> f b -> f (E a b)
  fplus fa fb = append (map L fa) (map R fb)


-- | either (map f) (map g) . decide = decide . map (either f g)
--   decide . map L = L
--   decide . map R = R
--
--  map (either absurd id) = either nonempty id . decide
class Decide f where
  nonempty :: f X -> a
  decide :: f (E a b) -> E (f a) (f b)
  decide = branch L R
  branch :: (f a -> c) -> (f b -> c) -> f (E a b) -> c
  branch f g e = case decide e of
    L fa -> f fa
    R fb -> g fb

instance Append [] where append = list'append

list'prepend,list'append :: [a] -> [a] -> [a]
list'prepend bs = go where
  go = \case
    [] -> bs
    a:as -> a : go as
list'append as bs = list'prepend bs as
