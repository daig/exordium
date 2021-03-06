{-# language UndecidableSuperClasses #-}
module X.ZSortMe.Indexed where
import X.Type.I
import X.Data.Maybe
import X.Kind.Type (Type)
import Prelude (Enum(..),(==))

class (Unindexed (Unindexed f) ~ Unindexed f
      ,Indexed i (Unindexed f)) => Indexed i f where
  type Unindexed f :: Type -> Type
  type Unindexed f = f
  indexed :: f a -> i -> Unindexed f a
  default indexed :: (Unindexed f ~ f) => f a -> i -> Unindexed f a
  indexed p _ = p

  
instance Indexed i I
instance Indexed i Maybe
instance Enum n => Indexed n [] where
  type Unindexed [] = Maybe
  indexed [] _ = Nothing 
  indexed (a:_) n | fromEnum n == 0 = Just a
  indexed (_:as) n = indexed as (pred n)
