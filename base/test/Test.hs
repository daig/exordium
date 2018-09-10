module Test where
import F32
import Prelude
import GHC.Types

main :: IO ()
main = print (W# (stgFloatToWord32 0.2#))
