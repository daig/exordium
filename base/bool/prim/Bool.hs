{-# language MagicHash #-}
module Bool where
import GHC.Types (RuntimeRep(IntRep))
import GHC.Prim
type R = IntRep
type Bool = Int#

(&&) :: Bool -> Bool -> Bool
(&&) = andI#
infixr 3 &&
(||) :: Bool -> Bool -> Bool
(||) = orI#
infixr 2 ||
not :: Bool -> Bool
not = (xorI# 1#)
bool :: a -> a -> Bool -> a
bool a b = \case {0# -> a; 1# -> b}
