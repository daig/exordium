{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module FingerTree (FingerTree(..,FTN), module X) where
import Measured as X
import FingerTree.Digit as X
import FingerTree.Node as X
import Prelude (Show,error)
import Cons
import Coerce (fromString)
import Eq

data FingerTree a
  = FT0
  | FT1 ~a
  | FTN# (Measure a) (Digit a) ~(FingerTree (Node a)) (Digit a)
deriving instance (Show a, Show (Measure a)) => Show (FingerTree a)
instance (Measured a, Def (Measure a)) => Measured (FingerTree a) where
  type Measure (FingerTree a) = Measure a
  measure = \case
    FT0 -> def
    FT1 a -> measure a
    FTN# v _ _ _ -> v

instance FoldMap FingerTree where
  foldMap f = \case
    FT0 -> zero
    FT1 x -> f x
    FTN# _ l t r -> foldMap f l + foldMap (foldMap f) t + foldMap f r


pattern FTN :: Measured a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
pattern FTN l t r <- FTN# _ l t r where
  FTN l t r = FTN# ((measure l + measure t) + measure r) l t r


-- TODO: make Iso
nodeToDigit :: Node a -> Digit a
nodeToDigit = \case
  Node2# _ a b -> Digit2 a b
  Node3# _ a b c -> Digit3 a b c

{-ftCons a = \case-}
  {-FT0 -> FT1 a-}
  {-FT1 b -> FTN (Digit1 a) FT0 (Digit1 b)-}
  {-FTN# v (Digit4 b c d e) !t r -> FTN# (measure a + v) (Digit2 a b) (Node3 c d e `nodeCons` t) r-}
  {-FTN# v l t r -> FTN# (measure a + v) (consDigit# l) t r-}
  {-where-}
    {-consDigit# = \case-}
      {-Digit1 b -> Digit2 a b-}
      {-Digit2 b c -> Digit3 a b c-}
      {-Digit3 b c d -> Digit4 a b c d-}
      {-Digit4{} -> error "consDigit#"-}
{-instance Snoc FingerTree where-}
  {-x |+ a = case x of-}
    {-FT0 -> FT1 a-}
    {-FT1 b -> FTN (Digit1 b) FT0 (Digit1 a)-}
    {-FTN# v l !t (Digit4 b c d e) -> FTN# (v + measure a) l (t |+ Node3 b c d) (Digit2 e a)-}
    {-FTN# v l t r -> FTN# (v + measure a) l t (snocDigit# r)-}
    {-where-}
      {-snocDigit# = \case-}
        {-Digit1 b -> Digit2 b a-}
        {-Digit2 b c -> Digit3 b c a-}
        {-Digit3 b c d -> Digit4 b c d a-}
        {-Digit4{} -> error "consDigit#"-}

