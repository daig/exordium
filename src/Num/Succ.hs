module Num.Succ where

-- | choice (L (R a)) = choice (R (L a))
class Choice x a where choice :: E x a -> a
class Choice x a => Decide x a where decide :: E a a -> x
class Succ a where succ :: a -> a
  {-choice :: E a a -> a -}
  {-choice = \case-}
    {-L a -> succ a-}
    {-R a -> a-}

{-class Succ a => Pred a where-}
  {-pred :: a -> a-}
  {-pred = choice -}
