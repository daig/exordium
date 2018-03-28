{-# language MagicHash #-}
module X.Kind.Nat (KnownNat,Nat,natVal) where
import qualified Prelude as P
import GHC.TypeLits hiding (natVal)
import X.Prim.Proxy

natVal :: forall n. KnownNat n => P.Integer
natVal = natVal' (proxy# :: Proxy# n)
