module Laws.Apply (module Laws.Apply, module X) where
import Class.Apply as X

{-assoc :: (Eq (f a), Apply f) => f a -> f a -> f a -> Bool-}
{-assoc a b c = (a|@|b)|@|c == a|@|(b|@|c)-}

{-mapPure :: forall f a b. (Eq (f b), Map f, Pure f) => (a -> b) -> a -> Bool-}
{-mapPure f a = map f (pure a) == pure @f (f a)-}
{-timesPure :: (Eq (f a), Apply f, Pure f) => f a -> a -> Bool-}
{-timesPure fa a = (fa |@| pure a) == fa && (pure a |@| fa) == fa-}
