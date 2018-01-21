module Int (module Int, module X) where
import Int.Type as X
import GHC.Num ((+),(*),(-))

int'plus,int'times,int'minus :: Int -> Int -> Int

int'plus = (+)
int'times = (*)
int'minus = (-)
