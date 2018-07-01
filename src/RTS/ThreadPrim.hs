{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language CPP #-}
module RTS.ThreadPrim where
import X.Prim.IO
import X.Prim.IO.Thread
import X.Prim.Bool

type CapacityId# = Int#
type ThreadStatus# = Int#

pattern Running# :: ThreadStatus#
pattern Running# = 0#

pattern BlockedOnMVar# :: ThreadStatus#
pattern BlockedOnMVar# = 1#

pattern BlockedOnBlackHole# :: ThreadStatus#
pattern BlockedOnBlackHole# = 2#

pattern BlockedOnSTM# :: ThreadStatus#
pattern BlockedOnSTM# = 6#
-- | Windows Only
pattern BlockedOnDoProc# :: ThreadStatus#
pattern BlockedOnDoProc# = 7#
-- | Only Relevant for @-threaded@ rts
pattern BlockedOnCCall# :: ThreadStatus#
pattern BlockedOnCCall# = 10#
-- | Same as @BlockedOnCCall@ but permit killing the worker thread
pattern BlockedOnCCall_Interruptible# :: ThreadStatus#
pattern BlockedOnCCall_Interruptible# = 11#
-- | Involved in a message sent to tso->msg_cap
pattern BlockedOnMsgThrowTo# :: ThreadStatus#
pattern BlockedOnMsgThrowTo# = 12#

-- | Thread is not on any run queues, but can be woken up by @tryWakeupThread()@
pattern Migrating# :: ThreadStatus#
pattern Migrating# = 13#

type ThreadInfo# =  (# State# RealWorld ,  ThreadStatus# ,  CapacityId# ,  Bool# #)
pattern ThreadInfo# :: State# RealWorld -> ThreadStatus# -> CapacityId# -> Bool# -> ThreadInfo#
pattern ThreadInfo# {stateToken, status#, capacity#, locked'#} =
                  (# stateToken, status#, capacity#, locked'# #)
threadInfo# :: ThreadId# -> State# RealWorld -> ThreadInfo#
threadInfo# = threadStatus#
