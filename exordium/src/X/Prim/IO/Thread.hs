{-# language MagicHash #-}
module X.Prim.IO.Thread
  (ThreadId#
  ,fork#, forkOn#
  ,killThread#
  ,yield#
  ,myThreadId#
  ,labelThread#
  ,isCurrentThreadBound#
  ,noDuplicate#
 -- | noDuplicate# tries to ensure that none of the thunks under
 -- evaluation by the current thread are also under evaluation by
 -- another thread.  It relies on *both* threads doing noDuplicate#;
 -- the second one will get blocked if they are duplicating some work.
 --
 -- The idea is that noDuplicate# is used within unsafePerformIO to
 -- ensure that the IO operation is performed at most once.
 -- noDuplicate# calls threadPaused which acquires an exclusive lock on
 -- all the thunks currently under evaluation by the current thread.
 --
 -- Consider the following scenario.  There is a thunk A, whose
 -- evaluation requires evaluating thunk B, where thunk B is an
 -- unsafePerformIO.  Two threads, 1 and 2, bother enter A.  Thread 2
 -- is pre-empted before it enters B, and claims A by blackholing it
 -- (in threadPaused).  Thread 1 now enters B, and calls noDuplicate#.
 --
 -- >    thread 1                      thread 2
 -- > +-----------+                 +---------------+
 -- > |    -------+-----> A <-------+-------        |
 -- > |  update   |   BLACKHOLE     | marked_update |
 -- > +-----------+                 +---------------+
 -- > |           |                 |               |
 -- >      ...                             ...
 -- > |           |                 +---------------+
 -- > +-----------+
 -- > |     ------+-----> B
 -- > |  update   |   BLACKHOLE
 -- > +-----------+
 --
 -- At this point: A is a blackhole, owned by thread 2.  noDuplicate#
 -- calls threadPaused, which walks up the stack and
 --
 --  - claims B on behalf of thread 1
 --  - then it reaches the update frame for A, which it sees is already
 --    a BLACKHOLE and is therefore owned by another thread.  Since
 --    thread 1 is duplicating work, the computation up to the update
 --    frame for A is suspended, including thunk B.
 --  - thunk B, which is an unsafePerformIO, has now been reverted to
 --    an AP_STACK which could be duplicated - BAD!
 --  - The solution is as follows: before calling threadPaused, we
 --    leave a frame on the stack (stg_noDuplicate_info) that will call
 --    noDuplicate# again if the current computation is suspended and
 --    restarted.
 --
 -- See the test program in concurrent/prog003 for a way to demonstrate
 -- this.  It needs to be run with +RTS -N3 or greater, and the bug
-- only manifests occasionally (once very 10 runs or so).
--
-- +------------------------+------------+----------+----------+
-- | Header row, column 1   | Header 2   | Header 3 | Header 4 |
-- | (header rows optional) |            |          |          |
-- +========================+============+==========+==========+
-- | body row 1, column 1   | column 2   | column 3 | column 4 |
-- +------------------------+------------+----------+----------+
-- | body row 2             | Cells may span columns.          |
-- +------------------------+------------+---------------------+
-- | body row 3             | Cells may  | \[                  |
-- +------------------------+ span rows. | f(n) = \sum_{i=1}   |
-- | body row 4             |            | \]                  |
-- +------------------------+------------+---------------------+


  ,threadStatus#) where
import GHC.Prim
