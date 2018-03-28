{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module X.Data.Struct.FingerTree.Node (Node(Node2,Node2#,Node3,Node3#), module X) where
import Prelude (Show)
import X.Functor.Map as X
import X.Num.Measured as X
import X.Functor.Fold as X
import X.Data.Struct.Unsafe
import X.Cast.Coerce.Unsafe
import X.Mono.Map

data Node a = Node2# (Measure a) ~a ~a | Node3# (Measure a) ~a ~a ~a

{-instance Map Node where-}
  {-map f = \case-}
   {-Node2# _ a b -> let (a',b') = (f a, f b) in Node2# (measure a' `add` measure b') a' b'-}
   {-Node3# _ a b c -> let (a',b', c') = (f a, f b, f c) in Node3# (measure a' `add` measure b' `add` measure c') a' b' c'-}

deriving instance (Show (Measure a), Show a) => Show (Node a)

instance Measured a => Measured (Node a) where
  type Measure (Node a) = Measure a
  measure (Node2# v _ _) = v
  measure (Node3# v _ _ _) = v

instance Fold1 Node where
  foldMap1 f = \case
    Node2# _ a b -> f a `add` f b
    Node3# _ a b c -> f a `add` f b `add` f c
instance Fold Node where foldMap = foldMap1

pattern Node2 :: Measured a => a -> a -> Node a
pattern Node2 a b <- Node2# _ a b where
  Node2 a b = Node2# (measure a `add` measure b) a b
pattern Node3 :: Measured a => a -> a -> a -> Node a
pattern Node3 a b c <- Node3# _ a b c where
  Node3 a b c = Node3# (measure a `add` measure b `add` measure c) a b c

{-# complete Node2, Node3 #-}
{-# complete Node2#, Node3 #-}
{-# complete Node2, Node3# #-}


instance Measured b => Monomap (Node b) (Node a) b a where
  _map = setter (\f -> \case
    Node2# _ a b -> Node2 (f a) (f b)
    Node3# _ a b c -> Node3 (f a) (f b) (f c))
-- | Only safe for measure-preserving functions
instance Map (Unsafe Node) where
  map f (Unsafe (Node2# v a b))   = Unsafe (Node2# (coerce# v) (f a) (f b))
  map f (Unsafe (Node3# v a b c)) = Unsafe (Node3# (coerce# v) (f a) (f b) (f c))
