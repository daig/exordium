{-# language TypeOperators, LambdaCase, EmptyCase #-}
module Void where

data X
absurd :: X -> a
absurd = \case {}
