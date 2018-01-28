module Zipping where
import Prism.Class as X
import Closed.Class as X
import Instances

newtype Zipping a b = Zipping {runZipping :: a -> a -> b}
syncTH
[instances| Closed Zipping where
  closed (Zipping z) = Zipping (\xa xa' x -> z (xa x) (xa' x))|]
