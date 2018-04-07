module X.Functor.FZero (FZero(..), module X) where
import X.Data.X as X
import X.Functor.Remap as X

-- | remap f g fzero = 
class Remap f => FZero f where
  fzero :: f X
  fzero = lose (\x -> x)
  lose :: (a -> X) -> f a
  lose f = remap f (\case {}) fzero

instance FZero [] where fzero = []
