{-# language MagicHash #-}
module X.TypeLevel where
import X.Kind.Nat
import X.Type.Int
import X.Num.Add
import Data.Foldable
import GHC.Exts
import qualified Data.Set as Set


{-class Sum (xs :: [Nat]) where sumFrom :: Integer -> Integer-}
{-sum :: forall ns. Sum ns => Integer-}
{-sum = sumFrom @ns 0-}
{-instance Sum '[] where-}
  {-sumFrom n = n-}
{-instance (Sum ns, KnownNat n) => Sum (n ': ns) where sumFrom n = sumFrom @ns (n `add` natVal @n)-}


test :: Set.Set Int
test = Set.fromDistinctAscList [1..]
{-test = foldr Set.insert Set.empty (build (\k z -> k 1 (k 2 (k 3 (k 4 z)))))-}
{-test = Data.Set.fromList [1,2,3,5] -}
{-sum' = \case-}
  {-[] -> 0-}
  {-x:xs -> add x (sum' xs)-}

-- {-# rules "sum" forall x xs. sum' (x:xs) = add x (sum' xs) #-}
-- {-# rules "sum0" sum' [] = 0 #-}
