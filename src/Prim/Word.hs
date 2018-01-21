{-# language MagicHash #-}
module Prim.Word
  (Word#, module X
  ,plusWord#, minusWord#, timesWord#
  ,plusWord2#, subWordC#, timesWord2#
  ,quotWord#, remWord#, quotRemWord#, quotRemWord2#
  ,and#, or#, xor#, not#
  ,uncheckedShiftL#, uncheckedShiftRL#
  ,word2Int#, word2Float#, word2Double#
  ,gtWord#, geWord#, eqWord#, neWord#, ltWord#, leWord#
  ,popCnt8#, popCnt16#, popCnt32#, popCnt64#, popCnt#
  ,clz8#, clz16#, clz32#, clz64#, clz#
  ,ctz8#, ctz16#, ctz32#, ctz64#, ctz#
  ,byteSwap16#, byteSwap32#, byteSwap64#, byteSwap#
  ,narrow8Word#, narrow16Word#, narrow32Word#) where
import GHC.Prim
import GHC.Prim as X (Int#,Float#,Double#)
