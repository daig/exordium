module X.Functor.Ordmap where


-- | imap ibc < ordmap ai ab = imap (\_ a -> ibc (ai a) (ab a))
class OrdMap i f where ordmap :: (a -> i) -> (a -> b) -> f a -> f b

