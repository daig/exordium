module Re.Type (Re(..), module X) where
import Map.Pro as X

newtype Re p s t a b = Re {runRe :: p b a -> p t s}

instance MapR p => ComapL (Re p s t) where colmap f (Re l) = Re (\p -> l (rmap f p))
instance ComapL p => MapR (Re p s t) where rmap f (Re l) = Re (\p -> l (colmap f p))
instance Promap p => Promap (Re p s t) where
  promap f g (Re l) = Re (\p -> l (promap g f p))

re :: (Re p s t s t -> Re p s t a b) -> p b a -> p t s
re l = runRe (l (Re (\p -> p)))
