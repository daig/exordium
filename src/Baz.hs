module Baz where
import Bazaar
{-import Class.Traverse_ as X-}
import Type.I
import Type.K
import Type.O


newtype Baz c t b a = Baz {runBaz :: forall f. c f => (a -> f b) -> f t}

sold :: c I => Baz c t a a -> t
sold m = case runBaz m I of {I t -> t}

baz'map :: (x -> a) -> Baz c t b x -> Baz c t b a
baz'map f (Baz t) = Baz (\afb -> t (\x -> afb (f x)))
{-baz'foldMap :: _ -> ( -> Baz c t b a -> m-}
baz'foldMap traverse = \f t -> case traverse (\x -> K (f x)) t of K m -> m

{-instance Traverse (Baz Map t b) where-}
  {-traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))-}
{-instance FoldMap0 (Baz Map t b) where foldMap0 = foldMap0Zeroault-}
{-instance Traverse0 (Baz Map t b) where-}
  {-traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))-}
{-instance FoldMap1 (Baz Map t b) where foldMap1 = foldMap1Zeroault-}
{-instance Traverse1 (Baz Map t b) where-}
  {-traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))-}
{-instance FoldMap_ (Baz Map t b) where-}
 {-foldMap_ = foldMap_Zeroault -}

{-instance Traverse_ (Baz Map t b) where-}
  {-traverse_ f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Map) (f x)))))-}

{-instance FoldMap (Baz Pure t b) where foldMap = foldMapZeroault-}
{-instance Traverse (Baz Pure t b) where-}
  {-traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))-}
{-instance FoldMap0 (Baz Pure t b) where foldMap0 = foldMap0Zeroault-}
{-instance Traverse0 (Baz Pure t b) where-}
  {-traverse0 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Pure) (f x)))))-}

{-instance FoldMap (Baz Apply t b) where foldMap = foldMapZeroault-}
{-instance Traverse (Baz Apply t b) where-}
  {-traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))-}
{-instance FoldMap1 (Baz Apply t b) where foldMap1 = foldMap1Zeroault-}
{-instance Traverse1 (Baz Apply t b) where-}
  {-traverse1 f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Apply) (f x)))))-}

{-instance FoldMap (Baz Applicative t b) where foldMap = foldMapZeroault-}
{-instance Traverse (Baz Applicative t b) where-}
  {-traverse f (Baz bz) = map (\(Bazaar m) -> Baz m) ((\(O fg) -> fg) (bz (\x -> O (map (sell @Applicative) (f x)))))-}
