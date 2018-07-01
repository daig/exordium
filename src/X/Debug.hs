module X.Debug where
import X.Prim.Exception
{-import GHC.Err-}

data Err where Err :: Show a => a -> Err
