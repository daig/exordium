module Each where
import Lens.Type as X (type (@~))
import Traverse

class Each s a b t | s -> a, t -> b, s b -> t, t a -> s where
  each :: (s @~ a) b t
  default each :: (Traverse f, s ~ f a, t ~ f b)  => (s @~ a) b t
  each = traverse

instance Each [a] a b [b] where
  each f = go where
    go = \case
      [] -> pure []
      (a:as) -> f a |@(:)@| go as

      <@(:)@>
