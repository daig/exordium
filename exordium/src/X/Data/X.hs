module X.Data.X where
import qualified Prelude as P

data X 

__ :: a
__ = P.undefined

{-absurd :: X -> a-}
{-absurd = \case {}-}

never :: a -> b
never _ = __
