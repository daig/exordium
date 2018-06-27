module X.Num.Add.Endo (Endo(..), {- _Endo2, -} module X) where
import X.Num.Add0 as X
import X.Arrow.Promap as X

newtype Endo a = Endo {runEndo :: a -> a}
  deriving anyclass Add0
instance Zero (Endo a) where zero = Endo (\a -> a)
instance Add (Endo a) where Endo f `add` Endo g = Endo (\x -> f (g x))

-- TODO: make a nice iso combinator for this
{-_Endo2 :: (Endo a -> Endo a -> Endo a) -> a -> a -> a-}
{-_Endo2 = promap Endo (promap Endo getEndo)-}
