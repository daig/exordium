module AllSatisfied (AllSatisfied, module X) where
import Constraint as X
import Trivial as X

type family AllSatisfied (cs :: [* -> Constraint]) (a :: *) = (c :: Constraint) | c -> cs a where
  AllSatisfied '[] a = Trivial a
  AllSatisfied (c ': cs) a = (c a, AllSatisfied cs a)
