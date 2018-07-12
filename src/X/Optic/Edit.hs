module X.Optic.Edit where
import X.Arrow.Mapped as X


{-newtype Edit a b s t = Edit {runEdit :: (a -> b) -> s -> t}-}

{-instance Promap (Edit a b) where promap sa bt (Edit abab) = Edit (\ab s -> bt (abab ab (sa s)))-}
{-instance Closed (Edit a b) where closed (Edit abst) = Edit (\ab xs x -> abst ab (xs x))-}
{-instance Traversed_ (Edit a b) where traversed_ = mapped-}
{-instance Traversed1 (Edit a b) where traversed1 = mapped-}
{-instance Traversed0 (Edit a b) where traversed0 = mapped-}
{-instance Traversed (Edit a b)  where traversed = mapped-}
{-instance Traversed' (Edit a b) where traversed' = mapped-}
{-instance Mapped (Edit a b) where setter abst (Edit abab) = Edit (\ab s -> abst (abab ab) s)-}
  {-{-grate g adj s =  (g (\get -> adj (get s)) )-}-}

{-_Edit :: Promap p => p (Edit a b a b) (Edit a b s t) -> p ((a -> b) -> a -> b) ((a -> b) -> s -> t)-}
{-_Edit = promap Edit runEdit-}


-- | An @Update@ inserts a single value into a container, possibly changing its type
newtype Update b s t = Update {runUpdate :: b -> s -> t}
instance Promap (Update b) where promap sa bt (Update bab) = Update (\b -> promap sa bt (bab b))
instance Map (Update b s) where map = postmap
instance Remap (Update b s) where remap _ = map
instance Strong (Update b s) where strong = map_strong
instance Comap (BA (Update b) t) where comap = premap_comap
instance Closed (Update b) where closed (Update f) = Update (\b xs x -> f b (xs x))
instance Traversed_ (Update b) where traversed_ = mapped
instance Traversed1 (Update b) where traversed1 = mapped
instance Traversed0 (Update b) where traversed0 = mapped
instance Traversed (Update b)  where traversed = mapped
instance Traversed' (Update b) where
  prism seta bt (Update bab) = Update (\b s -> case seta s of
    L t -> t
    R a -> bt (bab b a))
    -- traversed' = mapped
instance Mapped (Update b) where setter abst (Update bab) = Update (\b s -> abst (bab b) s)


-- | Update the values targeted by an optic, providing a combining function and value to insert.
_Update :: Promap p => p (Update b a b) (Update b s t) -> p (b -> a -> b) (b -> s -> t)
_Update = promap Update runUpdate

-- TODO: compare ergononomics and performance of _Update/update and set/set'
-- | Update the values targeted by an optic, providing a combining function and value to insert.
{-update :: (((a -> b) -> a -> b) -> ((a -> b) -> s -> t)) -> (b -> a -> b) -> b -> s -> t-}
{-update l bab b s = l (\ab -> ab) (bab b) s-}

set :: (Update b a b -> Update b s t) -> b -> s -> t
set = (`_Update` (\b _ -> b))

{-set' :: ((a -> b) -> s -> t) -> b -> s -> t-}
{-set' l x = l (\_ -> x)-}
