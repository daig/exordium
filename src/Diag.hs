{-# language TypeInType #-}
module Diag (Diag(..), module X) where
import NArgs as X

data family Diag (n :: Nat) :: NArgs n k * -> k -> *
newtype instance Diag 0 f a = Diag0 f
newtype instance Diag 1 f a = Diag1 (f a)
newtype instance Diag 2 f a = Diag2 (f a a)
newtype instance Diag 3 f a = Diag3 (f a a a)
