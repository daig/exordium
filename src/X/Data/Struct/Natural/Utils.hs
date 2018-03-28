{-# OPTIONS_HADDOCK not-home #-}
module X.Data.Struct.Natural.Utils
  (Natural(..)
  ,isValidNatural
  ,naturalFromInteger,wordToNatural,naturalToWordMaybe
  ,minusNaturalMaybe
  ,powModNatural
  ) where
import GHC.Natural
