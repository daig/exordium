module X.Syntax.FromInteger.Word (fromInteger, module X) where
import X.Data.Struct.Integer as X
import X.Type.Word.W as X
import qualified X.Stock as Stock

-- | Monomorphic "fromInteger" for use with @-XRebindableSyntax@ which requires explicit conversion.
fromInteger :: Integer -> Word
fromInteger = Stock.fromInteger 
