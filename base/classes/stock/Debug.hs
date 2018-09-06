module Debug (Debug,debug,debugLn,debugErrLn) where
import qualified Debug.Trace as GHC
import GHC.Show
import GHC.Debug
import Prelude (Char,IO)

type Debug = Show

debug :: Debug a => a -> [Char]
{-# inline debug #-}
debug = show

trace :: [Char] -> a -> a
{-# inline trace #-}
trace = GHC.trace

traceDebug :: Debug a => a -> b -> b
{-# inline traceDebug #-}
traceDebug = GHC.traceShow

traceDebugId :: Debug a => a -> a
{-# inline traceDebugId #-}
traceDebugId = GHC.traceShowId

traceStack :: [Char] -> a -> a
{-# inline traceStack #-}
traceStack = GHC.traceStack

traceEvent :: [Char] -> a -> a
{-# inline traceEvent #-}
traceEvent = GHC.traceEvent

traceEventIO :: [Char] -> IO ()
{-# inline traceEventIO #-}
traceEventIO = GHC.traceEventIO

traceMarker :: [Char] -> a -> a
{-# inline traceMarker #-}
traceMarker = GHC.traceMarker

traceMarkerIO :: [Char] -> IO ()
{-# inline traceMarkerIO #-}
traceMarkerIO = GHC.traceMarkerIO
