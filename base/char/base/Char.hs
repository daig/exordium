module Char (module Char ,module Data.Char) where
import Data.Char
import GHC.Types (RuntimeRep(..))
import Bool
import Prelude (Ord(..),Eq(..))
type R = LiftedRep

eq, ne, gt, ge, lt, le :: Char -> Char -> Bool.Bool
eq = (==)
ne = (/=)
gt = (>)
ge = (>=)
lt = (<)
le = (<=)
