module Monad.T (TMonad(..), module X) where
import Map.T as X
import Monad as X

class TMap t => MonadT t where
  tbind :: Monad n => (m --> t n) -> (t m --> t n)
  tbind mtn tm = tjoin (tmap mtn tm)
  tjoin :: Monad m => t (t m) --> t m
  tjoin = tbind (\x -> x)


