module Test where
import Int (minBound,maxBound,add,print)
import Prelude (return, IO)

main :: IO ()
main = print (add minBound' (add minBound' (add maxBound' minBound')))
  where
      minBound' = minBound ()
      maxBound' = maxBound ()
