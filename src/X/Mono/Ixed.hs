module X.Mono.Ixed where
import X.Arrow.Traversed

class Ixed i a s | s -> i, s -> a where ixed :: Traversed p => i -> p a a -> p s s
class Ixed i a s => Ixed0 i a s | s -> i, s -> a where ixed0 :: Traversed0 p => i -> p a a -> p s s
class Ixed i a s => Ixed1 i a s | s -> i, s -> a where ixed1 :: Traversed1 p => i -> p a a -> p s s
class (Ixed0 i a s,Ixed1 i a s) => Ixed_ i a s | s -> i, s -> a where
  ixed_ :: Traversed_ p => i -> p a a -> p s s
