{-# language MagicHash #-}
module Prim.Addr 
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
import Prim.Char as X (Char#)
import Prim.Int as X (Int#)
import Prim.Word as X (Word#)
import Prim.Float as X (Float#)
import Prim.Double as X (Double#)
import Prim.StablePtr as X (StablePtr#)
import Prim.IO as X (State#)

{-# DEPRECATED addr2Int#, int2Addr# "Addr are not Int" #-}
addr2Int# :: Addr# -> Int#
addr2Int# = Prim.addr2Int#

int2Addr# :: Int# -> Addr#
int2Addr# = Prim.int2Addr#
