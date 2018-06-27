module X.Num.Add.Endo (Endo(..), Endo0(..), {- _Endo2, -} module X) where
import X.Num.Add0 as X
import X.Arrow.Promap as X
import X.Data.Maybe

newtype Endo a = Endo {runEndo :: a -> a}
  deriving anyclass Add0
instance Zero (Endo a) where zero = Endo (\a -> a)
instance Add (Endo a) where Endo f `add` Endo g = Endo (\x -> f (g x))

-- TODO: make a nice iso combinator for this
{-_Endo2 :: (Endo a -> Endo a -> Endo a) -> a -> a -> a-}
{-_Endo2 = promap Endo (promap Endo getEndo)-}

newtype Endo0 a = Endo0 {runEndo0 :: Maybe a -> a}
instance Add (Endo0 a) where
  Endo0 f `add` Endo0 g = Endo0 (\x' -> f (Just (g x')))
