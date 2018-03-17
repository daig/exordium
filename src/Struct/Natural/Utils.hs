{-# OPTIONS_HADDOCK not-home #-}
module Struct.Natural.Utils
  (Natural(..)
  ,isValidNatural
  ,naturalFromInteger,wordToNatural,naturalToWordMaybe
  ,minusNaturalMaybe
  ,powModNatural
  ) where
import GHC.Natural
