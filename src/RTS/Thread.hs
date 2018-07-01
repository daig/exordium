{-# language MagicHash, UnboxedTuples #-}
{-# language BlockArguments #-}
module RTS.Thread where
import X.Prim.IO.Thread
import X.Type.IO
import X.Prim.Int
import X.Type.Int
import qualified Prelude  as P
import X.Data.Bool
import X.Prim.Any

{-runIO# :: IO a -> State# RealWorld -> (# State# RealWorld, a #)-}
{-{-# INLINE runIO# #-}-}
{-runIO# (IO io) = io-}
{-data ThreadId = ThreadId# ThreadId#-}
{-data ThreadStatus = ThreadStatus# ThreadStatus#-}
{-data CapacityId = CapacityId# CapacityId#-}
{-pattern Running :: ThreadStatus-}
{-pattern Running = ThreadStatus# Running#-}
{-data ThreadInfo = ThreadInfo {status :: {-# UNPACK #-} ThreadStatus-}
                             {-,capacity :: {-# UNPACK #-} CapacityId-}
                             {-,locked' :: {-# UNPACK #-} Bool}-}
{-threadInfo#, threadInfo'# :: State# RealWorld -> ThreadId# -> Int-}
{-{-# INLINE CONLIKE threadInfo# #-}-}
{-{-# INLINE CONLIKE threadInfo'# #-}-}
{-threadInfo# s i = case threadStatus# i s of-}
  {-ThreadInfo# {status# = Running#} -> 1-}
  {-_ -> 0-}
{-threadInfo'# s i = case threadStatus i `runIO#` s of-}
  {-(# _, Running #) -> 1-}
  {-_ -> 0-}
{-threadInfo' :: ThreadId# -> IO Int-}
{-threadInfo' i = threadStatus i P.>>= \case {Running -> P.return 1; _ -> P.return 0}-}
{-{-threadInfo# :: ThreadId# -> IO -}-}
{-{-getThreadStatus :: ThreadId -> IO ThreadInfo-}-}
{-{-getThreadStatus ::  -}-}
{-threadStatus :: ThreadId# -> IO ThreadStatus-}
{-{-# INLINE threadStatus #-}-}
{-threadStatus i = IO \s -> case threadStatus# i s of (# s', stat, _, _ #) -> (# s', ThreadStatus# stat #)-}
{-threadInfo :: ThreadId -> IO ThreadInfo-}
{-threadInfo (ThreadId# ii) = IO \s -> case threadStatus# ii s of-}
  {-(# s', stat, cap , locked' #) -> (# s', ThreadInfo (ThreadStatus# stat)-}
                                                     {-(CapacityId# cap)-}
                                                     {-(tagToEnum# locked') #)-}
