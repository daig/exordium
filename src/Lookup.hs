module Lookup where
import Type.Option


class Index f where
  type Ix f :: *
  lookup :: f a -> Ix f -> (?) a
  {-lookup :: Def m => f a -> (Ix f -> a -> m) -> m-}

class Tabulate0 f where
  {-lookup = foldOf indexed-}
  tabulate0 :: (Ix f -> (?) a) -> f a
  {-tabulate0 = review indexed-}
