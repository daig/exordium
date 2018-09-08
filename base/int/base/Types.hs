module Types (module X, module Types) where
import GHC.Prim
import GHC.Types as X (RuntimeRep(..),IO)
import qualified Prelude
import qualified GHC.Int as GHC

type R = LiftedRep
type PairR r1 r2 = LiftedRep
type a * b = (a,b)
type BoolR = LiftedRep
type IsizeR = LiftedRep
type UsizeR = LiftedRep

-- $unsafe
-- GHC does not enforce this type, so the operations assume they are passed a value within the valid range, but will correctly narrow the return type value.
