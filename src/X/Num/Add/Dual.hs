module X.Num.Add.Dual (Dual(..), {- _Dual2, -} module X) where
import X.Num.Add0 as X
import X.Arrow.Promap as X

newtype Dual a = Dual {getDual :: a}
  deriving anyclass Add0
  deriving newtype Zero
instance Add a => Add (Dual a) where Dual a `add` Dual b = Dual (add b a)

-- TODO: make a nice iso combinator for this
{-_Dual2 :: (Dual a -> Dual a -> Dual a) -> a -> a -> a-}
{-_Dual2 = promap Dual (promap Dual getDual)-}
