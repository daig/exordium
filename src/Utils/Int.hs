module Utils.Int (module Utils.Int, module X) where
import Type.Int as X
import GHC.Num ((+),(*),(-))

int'plus,int'times,int'minus :: Int -> Int -> Int

int'plus = (+)
int'times = (*)
int'minus = (-)
