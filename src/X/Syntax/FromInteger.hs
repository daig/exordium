{-# language MagicHash #-}
-- | Use with @-XRebindableSyntax@ for numeric literals
-- 
-- >>> 10
-- fromInteger (10 :: Integer)
--
-- >>> - 3
-- negate (fromInteger (3 :: Integer))
--
-- but with @-XNegativeLiterals@, the space matters!
--
-- >>> -3 
-- fromInteger ((-3) :: Integer)
--
-- >>> - 3
-- negate (fromInteger (3 :: Integer))
--
-- with @-XHexLiterals@:
--
-- >>> 0xDEADBEEF
-- fromInteger 3735928559
--
-- and with @-XBinaryLiterals@:
--
-- >>> 0b101010
-- fromInteger (42 :: Integer)
--
-- In some cases, you may not need or want polymorphic numeric literals.
-- In that case, use one of the submodules like 'X.Num.FromInteger.Int',
-- Either by hiding this module or shadowing locally
--
-- > import qualified X.Syntax.FromInteger.Int as Int
-- > x = let fromInteger = Int.fromInteger in 3 + 5
--
module X.Syntax.FromInteger(FromInteger#(..),Integer) where
import X.Data.Struct.Integer

class FromInteger# i where fromInteger :: Integer -> i
instance FromInteger# Integer where fromInteger i = i
