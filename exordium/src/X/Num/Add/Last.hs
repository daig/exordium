module X.Num.Add.Last (Last(..), module X) where
import X.Num.Add0 as X

newtype Last a = Last {getLast :: a}
  deriving anyclass Add0
  deriving newtype Zero
instance Add (Last a) where _ `add` Last b = Last b
