module Lookup where
import Maybe.Type


class Index f where
  type Ix f :: *
  lookup :: f a -> Ix f -> Maybe a
  {-lookup :: Def m => f a -> (Ix f -> a -> m) -> m-}

class Tabulate0 f where
  {-lookup = foldOf indexed-}
  tabulate0 :: (Ix f -> Maybe a) -> f a
  {-tabulate0 = review indexed-}
