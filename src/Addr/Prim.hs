{-# language MagicHash #-}
module Addr.Prim (addr2Int#,int2Addr#,module X) where
import GHC.Prim as X
  (Addr#, Char#, Int#, Word#, Float#, Double#, StablePtr#, State#
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
  ,indexAddrOffAddr#, readAddrOffAddr#)
import qualified GHC.Prim as Prim

{-# DEPRECATED addr2Int#, int2Addr# "Addr are not Int" #-}
addr2Int# :: Addr# -> Int#
addr2Int# = Prim.addr2Int#

int2Addr# :: Int# -> Addr#
int2Addr# = Prim.int2Addr#
