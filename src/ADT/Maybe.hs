{-# GHC_OPTIONS -wno-orphans #-}
module ADT.Maybe (Maybe(..),maybe) where
import GHC.Base (Maybe(..))



maybe :: r -> (a -> r) -> Maybe a -> r
maybe z f = \case
  Nothing -> z
  Just x -> f x
