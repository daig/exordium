{-# language MagicHash #-}
module Prim.Word.Utils
  (gcdWord
  ,powModWord
  ,recipModWord
  ,testPrimeWord#, nextPrimeWord#
  ,sizeInBaseWord#
  ,exportWordToAddr, exportWordToMutableByteArray
  ) where
import GHC.Integer.GMP.Internals
