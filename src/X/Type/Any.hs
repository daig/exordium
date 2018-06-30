{-# language MagicHash #-}
module X.Type.Any (Any#,Any) where
import qualified GHC.Types as GHC
import X.Prim.Any
import X.Prim.Int
import GHC.Classes (Eq(..))

type Any# = GHC.Any
newtype Any = Any# GHC.Any

-- | Check the underlying pointers for equality. values must be in WHNF to be somewhat reliable, but the GC can  still move values unpredictably.
instance Eq Any where
  Any# a == Any# b = tagToEnum# (reallyUnsafePtrEquality# a b)
  Any# a /= Any# b = tagToEnum# (1# `xorI#` reallyUnsafePtrEquality# a b)
