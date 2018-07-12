module TH.Lit (Lit(..),module X) where
import Language.Haskell.TH.Syntax
import X.Type.Char as X
import X.Data.Struct.Integer as X
import GHC.Real as X (Ratio(..)) -- TODO: factor this out
