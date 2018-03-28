{-# OPTIONS_HADDOCK not-home #-}
module X.Struct.Natural.Utils
  (Natural(..)
  ,isValidNatural
  ,naturalFromInteger,wordToNatural,naturalToWordMaybe
  ,minusNaturalMaybe
  ,powModNatural
  ) where
import GHC.Natural
