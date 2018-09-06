module X.Functor.FPlus0 (FPlus0(..),module X) where
import X.Functor.Remap as X
import X.Data.E as X
import X.Functor.Map
import X.Functor.FPlus as X
import X.Functor.FZero as X

-- | remap R unR (fplus fzero a) = remap L unL (fplus a fzero) = a
class (FPlus f, FZero f) => FPlus0 f

instance FPlus0 []

{-list'prepend,list'append :: [a] -> [a] -> [a]-}
{-list'prepend bs = go where-}
  {-go = \case-}
    {-[] -> bs-}
    {-a:as -> a : go as-}
{-list'append as bs = list'prepend bs as-}
