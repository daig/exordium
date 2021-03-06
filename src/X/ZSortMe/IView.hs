module X.ZSortMe.IView where
{-import X.ZSortMe.Indexable.Class-}
{-import X.Optic.View-}
{-import X.Type.K-}
{-import X.Functor.Bifold-}

{-newtype IView i r a b = IView {runIView :: i -> a -> r}-}
{-{-_IView :: Promap p => p (a -> r) (a' -> r') -> p (IView r a b) (IView r' a' b')-}-}
{-{-_IView = promap runIView IView-}-}
{--- _IView :: Promap p => p (IView r a b) (IView r' a' b') -> p (a -> r) (a' -> r')-}
{--- _IView = promap IView runIView-}
{-instance Traversed_ (IView i r) where-}
  {-_1 (IView iz) = IView (\i (a,_) -> iz i a)-}
  {-traversal_ l (IView iar) = IView (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})-}
{-instance Promap (IView i r) where-}
  {-promap f _ (IView iz) = IView (\i -> premap f (iz i))-}
{-instance Map (IView i r a) where map = postmap-}
{-instance BiComap (IView i r) where-}
  {-bicomap f _ (IView iz) = IView (\i -> premap f (iz i))-}
{-instance Comap (IView i r a) where comap = cormap-}

{-instance Zero r => Traversed' (IView i r) where-}
  {-_L (IView iz) = IView (\i -> bifoldMap_ (iz i) (\_ -> zero))-}

{-instance Add0 r => Traversed (IView i r) where-}
  {-traversal l (IView iar) = IView (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})-}
{-instance Zero r => Traversed0 (IView i r) where-}
  {-traversal0 l (IView iar) = IView (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})-}
{-instance Add r => Traversed1 (IView i r) where-}
  {-traversal1 l (IView iar) = IView (\i s -> case (l (\a -> K (iar i a))) s of {K r -> r})-}

{-instance Loop' (IView i r) where-}
  {-un_L (IView ir) = IView (\i a -> ir i (L a))-}
  {-un_R (IView ir) = IView (\i a -> ir i (R a))-}

{-instance (j ~ i) => Indexed j (IView i r) where-}
  {-type Unindexed (IView i r) = View r-}
  {-indexed (IView iar) i = View (iar i)-}
{-instance Indexed i (View r)-}
