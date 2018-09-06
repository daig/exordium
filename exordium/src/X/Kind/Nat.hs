{-# language MagicHash #-}
module X.Kind.Nat (KnownNat,Nat,natVal,natValI#,Integer) where
import GHC.TypeLits hiding (natVal)
import X.Prim.Proxy
import X.Type.Int
import X.Data.Struct.Integer
import X.Cast.Coerce.Unsafe

natVal :: forall n. KnownNat n => Integer
natVal = natVal' (proxy# :: Proxy# n)
-- | Only safe for nats int the range of @maxBound \@Int@
natValI# :: forall n. KnownNat n => Int
{-# INLINE CONLIKE natValI# #-}
natValI# = coerce# (natVal' (proxy# :: Proxy# n)) 
