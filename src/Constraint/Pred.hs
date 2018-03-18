-- {-# language OverlappingInstances, IncoherentInstances #-}
-- {-# language NoMonomorphismRestriction #-}
module Constraint.Pred where
import ADT.Maybe
import Prelude
import Constraint.Witness
import Cast

class Pred' c ~ flag => Test (c :: Constraint) (flag :: Bool) where tt :: (c => r) -> r -> r
instance Pred' c ~ flag => Test c flag where tt _ r = r
instance (Pred' c ~ 'True, c) => Test c 'True where tt l _ = l
class Pred (c :: Constraint) where
  type Pred' c :: Bool
  type Pred' c = 'True
instance Pred c where type Pred' c = 'False
{-instance {-# incoherent #-} Pred (Show Int) where type Pred' (Show Int) = 'True-}

