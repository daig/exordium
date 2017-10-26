{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module FingerTree.Node (Node(Node2,Node2#,Node3,Node3#), module X) where
import Prelude (Show)
import Measured as X
import RelFoldMap as X

data Node a = Node2# (Measure a) ~a ~a | Node3# (Measure a) ~a ~a ~a

deriving instance (Show (Measure a), Show a) => Show (Node a)

instance Measured a => Measured (Node a) where
  type Measure (Node a) = Measure a
  measure (Node2# v _ _) = v
  measure (Node3# v _ _ _) = v

instance RelFoldMap Node where
  foldMap1 f = \case
    Node2# _ a b -> f a + f b
    Node3# _ a b c -> f a + f b + f c
instance FoldMap Node where foldMap = foldMap1

pattern Node2 :: Measured a => a -> a -> Node a
pattern Node2 a b <- Node2# _ a b where
  Node2 a b = Node2# (measure a + measure b) a b
pattern Node3 :: Measured a => a -> a -> a -> Node a
pattern Node3 a b c <- Node3# _ a b c where
  Node3 a b c = Node3# (measure a + measure b + measure c) a b c