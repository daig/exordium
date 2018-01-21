{-# language MagicHash #-}
module Addr.Prim 
  (Addr#, module X
  ,nullAddr#
  ,plusAddr#, minusAddr#,remAddr#
  ,gtAddr#, geAddr#, eqAddr#, neAddr#, ltAddr#, leAddr#
  ,indexCharOffAddr#, readCharOffAddr#
  ,indexWideCharOffAddr#, readWideCharOffAddr#
  ,indexInt8OffAddr# ,indexInt16OffAddr# ,indexInt32OffAddr# ,indexInt64OffAddr#, indexIntOffAddr#
  ,readInt8OffAddr# ,readInt16OffAddr# ,readInt32OffAddr# ,readInt64OffAddr#, readIntOffAddr#
  ,indexWord8OffAddr# ,indexWord16OffAddr# ,indexWord32OffAddr# ,indexWord64OffAddr#, indexWordOffAddr#
  ,readWord8OffAddr# ,readWord16OffAddr# ,readWord32OffAddr# ,readWord64OffAddr#, readWordOffAddr#
  ,indexFloatOffAddr#, readFloatOffAddr#
  ,indexDoubleOffAddr#, readDoubleOffAddr#
  ,indexStablePtrOffAddr#, readStablePtrOffAddr#
  ,indexAddrOffAddr#, readAddrOffAddr#
  ,addr2Int#,int2Addr#) where
import GHC.Prim hiding (addr2Int#, int2Addr#)
import qualified GHC.Prim as Prim
import Char.Prim as X (Char#)
import Int.Prim as X (Int#)
import Word.Prim as X (Word#)
import Float.Prim as X (Float#)
import Double.Prim as X (Double#)
import StablePtr.Prim as X (StablePtr#)
import IO.Prim as X (State#)

{-# DEPRECATED addr2Int#, int2Addr# "Addr are not Int" #-}
addr2Int# :: Addr# -> Int#
addr2Int# = Prim.addr2Int#

int2Addr# :: Int# -> Addr#
int2Addr# = Prim.int2Addr#
