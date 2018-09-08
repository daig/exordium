module Enum (Enum(..)) where
import Prelude (map,Int,(+),subtract)
class  Enum a   where
    -- | the successor of a value.  For numeric types, 'succ' adds 1.
    succ                :: a -> a
    -- | the predecessor of a value.  For numeric types, 'pred' subtracts 1.
    pred                :: a -> a
    -- | Convert from an 'Int'.
    toEnum              :: Int -> a
    -- | Convert to an 'Int'.
    -- It is implementation-dependent what 'fromEnum' returns when
    -- applied to a value that is too large to fit in an 'Int'.
    fromEnum            :: a -> Int

    -- | Used in Haskell's translation of @[n..]@.
    enumFrom            :: a -> [a]
    -- | Used in Haskell's translation of @[n,n'..]@.
    enumFromThen        :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n..m]@.
    enumFromTo          :: a -> a -> [a]
    -- | Used in Haskell's translation of @[n,n'..m]@.
    enumFromThenTo      :: a -> a -> a -> [a]

    succ                   = \x -> toEnum ((+ 1)  ( fromEnum x))
    pred                   = \x -> toEnum ((subtract 1) (fromEnum x))
    enumFrom x             = map toEnum [fromEnum x ..]
    enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
    enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]
