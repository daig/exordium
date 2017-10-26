module NArgs (NArgs, module X) where
import GHC.TypeLits as X (Nat)
import Data.Kind as X (type (*))

type family NArgs (n :: Nat) (i :: k) (o :: k')
type instance NArgs 0 i o = o
type instance NArgs 1 i o = i -> o
type instance NArgs 2 i o = i -> i -> o
type instance NArgs 3 i o = i -> i -> i -> o
type instance NArgs 4 i o = i -> i -> i -> i -> o
type instance NArgs 5 i o = i -> i -> i -> i -> i -> o
