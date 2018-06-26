module X.Syntax.FromInteger.Int (fromInteger, module X) where
import X.Data.Struct.Integer as X
import X.Type.Int.I as X
import qualified X.Stock as Stock

-- | Monomorphic "fromInteger" for use with @-XRebindableSyntax@ which requires explicit conversion.
fromInteger :: Integer -> Int
fromInteger = Stock.fromInteger 
