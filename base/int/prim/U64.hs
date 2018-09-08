module U64 (Word,U64#,module X) where
import Types
import U64.Native as X
import U64.Shim as X

type Word = U64#
