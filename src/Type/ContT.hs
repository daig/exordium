module Type.ContT where
{-import Class.TMap as X-}

newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r}

{-mapIsoT :: (n ~~= m) -> ContT r m ~~= ContT r n-}
{-mapIsoT l = dimap (go l) (go (re l)) where-}
  {-go :: (f ~~= g) -> ContT r g --> ContT r f-}
  {-go l' (ContT k) = ContT (\anr -> review l' (k (\a -> l' `view` anr a)))-}
