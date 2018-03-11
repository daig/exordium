{-# language MagicHash #-}
module Nat (KnownNat,Nat,natVal) where
import qualified Prelude as P
import GHC.TypeLits hiding (natVal)
import Prim.Proxy

natVal :: forall n. KnownNat n => P.Integer
natVal = natVal' (proxy# :: Proxy# n)
