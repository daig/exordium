{-# OPTIONS_HADDOCK not-home #-}
{-# language MagicHash #-}
module X.Data.Struct.Integer.Utils
  (Integer(..)
  ,isValidInteger#
  ,mkInteger,smallInteger,wordToInteger
  ,integerToWord,integerToInt
  ,encodeFloatInteger,floatFromInteger
  ,encodeDoubleInteger,doubleFromInteger
  ,plusInteger,minusInteger,timesInteger
  ,negateInteger,absInteger,signumInteger
  ,divModInteger,divInteger,modInteger
  ,quotRemInteger,quotInteger,remInteger
  ,eqInteger,neqInteger,compareInteger
  ,leInteger,gtInteger,ltInteger,geInteger
  ,eqInteger#,neqInteger#
  ,leInteger#,gtInteger#,ltInteger#,geInteger#
  -- * Bit-operations
  ,andInteger,orInteger,xorInteger
  ,complementInteger
  ,shiftLInteger, shiftRInteger
  ,testBitInteger ,bitInteger
  ,hashInteger
  ,popCountInteger
  ,gcdInteger, gcdExtInteger, lcmInteger
  ,sqrInteger
  ,powModInteger
  ,recipModInteger
  ,wordToNegInteger
  ,bigNatToInteger,bigNatToNegInteger
  ,testPrimeInteger, nextPrimeInteger
  ,sizeInBaseInteger
  ,exportIntegerToAddr, importIntegerFromAddr
  ,exportIntegerToMutableByteArray, importIntegerFromByteArray
  ,module X
  ) where
import GHC.Integer.GMP.Internals
import X.Data.Struct.Integer as X
