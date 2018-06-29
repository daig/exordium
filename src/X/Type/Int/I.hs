{-# language MagicHash #-}
module X.Type.Int.I (Int(..), shiftL,shiftR, module X) where
import GHC.Int
import X.Stock.Ord as X
import X.Prim.Int

shiftL, shiftR :: Int -> Int -> Int -- TODO: add these to classes somehow
shiftL (I# i) (I# j) = I# (uncheckedIShiftL# i j)
shiftR (I# i) (I# j) = I# (uncheckedIShiftRA# i j)
