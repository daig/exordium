module Fan (type (:&:)(..), module X)where
import Monad as X

data (f :&: g) a = f a :&: g a

instance (Map f, Map g) => Map (f :&: g) where
  map f (a :&: b) = map f a :&: map f b
instance (Pure f, Pure g) => Pure (f :&: g) where
  pure a = (pure a :&: pure a)
instance (Apply f, Apply g) => Apply (f :&: g) where
  (f :&: g) |$| (a :&: b) = (f |$| a) :&: (g |$| b)
-- TODO: this is a bad Bind instance. Does it break laws? Can we do better?
instance (Bind m, Bind n) => Bind (m :&: n) where
  f =<< (m :&: n) =
    ((\x -> case f x of {m' :&: _ -> m'}) =<< m)
    :&: ((\x -> case f x of {_ :&: n' -> n'}) =<< n)
