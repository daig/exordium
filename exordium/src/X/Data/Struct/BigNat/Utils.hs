{-# OPTIONS_HADDOCK not-home #-}
{-# language MagicHash #-}
module X.Data.Struct.BigNat.Utils
  (BigNat(..)
  ,isValidBigNat#
  ,sizeofBigNat#
  ,zeroBigNat,oneBigNat ,nullBigNat
  ,byteArrayToBigNat#
  ,wordToBigNat ,wordToBigNat2
  ,bigNatToInt, bigNatToWord
  ,indexBigNat#
  ,plusBigNat,minusBigNat
  ,plusBigNatWord ,minusBigNatWord
  ,timesBigNat,timesBigNatWord ,sqrBigNat
  ,quotRemBigNat ,quotRemBigNatWord
  ,quotBigNat, quotBigNatWord
  ,remBigNat, remBigNatWord
  ,gcdBigNat, gcdBigNatWord
  ,powModBigNat, powModBigNatWord
  ,recipModBigNat
  ,shiftRBigNat,shiftLBigNat
  ,testBitBigNat
  ,andBigNat,xorBigNat ,orBigNat
  ,popCountBigNat
  ,bitBigNat
  ,isZeroBigNat ,isNullBigNat#
  ,compareBigNat ,compareBigNatWord
  ,eqBigNat, eqBigNatWord, eqBigNatWord#
  ,gtBigNatWord#
  ,testPrimeBigNat, nextPrimeBigNat
  ,sizeInBaseBigNat
  ,exportBigNatToAddr, importBigNatFromAddr
  ,exportBigNatToMutableByteArray, importBigNatFromByteArray
  ) where

import GHC.Integer.GMP.Internals
