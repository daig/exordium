{-# language UndecidableSuperClasses #-}
module Arrow.Coindexed (Coindexed(..), module X) where
import Arrow.Promap as X
import ADT.E as X

class (Coindexed e q q, Promap p) => Coindexed e q p where
  coindexed :: p a b -> q a (E e b)
  default coindexed :: p ~ q => p a b -> q a (E e b)
  coindexed = postmap R


instance Coindexed e (->) (->)

