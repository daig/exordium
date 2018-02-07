module Empty (module Empty, module X) where
import Empty.Class as X

empty_zero :: Empty f => f a
empty_zero = empty
