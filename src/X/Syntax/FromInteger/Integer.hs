module X.Syntax.FromInteger.Integer (fromInteger, module X) where
import X.Data.Struct.Integer as X

-- | Monomorphic "fromInteger" for use with @-XRebindableSyntax@ which requires explicit conversion.
fromInteger :: Integer -> Integer
fromInteger i = i
