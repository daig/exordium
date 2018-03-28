{-# language UndecidableSuperClasses #-}
module X.Arrow.Coindexed (Coindexed(..), module X) where
import X.Arrow.Promap as X
import X.ADT.E as X

class (Coindexed e q q, Promap p) => Coindexed e q p where
  coindexed :: p a b -> q a (E e b)
  default coindexed :: p ~ q => p a b -> q a (E e b)
  coindexed = postmap R


instance Coindexed e (->) (->)

