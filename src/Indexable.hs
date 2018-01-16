{-# language UndecidableSuperClasses #-}
module Indexable where
import Traversal
import Int
import Category

{-class (Unindexed (Unindexed p) ~ Unindexed p, IndexedP i (Unindexed p)) => IndexedP i p where-}
  {-type Unindexed p :: * -> * -> *-}
  {-indexP :: p a b -> i -> Unindexed p a b-}

{-instance IndexedP i (->) where-}
  {-type Unindexed (->) = (->)-}
  {-indexP = \a _ -> a-}

{-type IndexedTraversal i s a b t = forall p. (IndexedP i p, Traversal p, Traversal (Unindexed p))-}
                               {-=> p a b -> Unindexed p s t-}

{-newtype IFun i a b = IFun {runIFun :: i -> a -> b}-}
{-instance (i ~ j) => IndexedP i (IFun j) where-}
  {-type Unindexed (IFun j) = (->)-}
  {-indexP = runIFun-}

{-foo :: IndexedTraversal Int [a] a b [b]-}
{-foo pab = go (0 :: Int) where-}
  {-go i = traversing (indexP pab i)-}


{-_Cons :: Choice p => p (a,[a]) (b,[b]) -> p [a] [b]-}
{-_Cons = prism (\(a,as) -> a:as) (\case {[] -> L []; a:as -> R (a,as)})-}
{-_Nil :: Choice p => p () () -> p [a] [a]-}
{-_Nil = prism (\() -> []) (\case {[] -> R (); as -> L as})-}

{-imap :: IFun Int a b -> [a] -> [b]-}
{-imap (IFun f) = go 0 where-}
  {-go i = \case-}
    {-[] -> []-}
    {-a:as -> f i a : go (i+1) as-}

{-bar :: Traversal p => p a b -> p [a] [b]-}
{-bar = wander traverse-}
