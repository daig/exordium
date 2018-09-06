module X.Functor.TraverseA where
import X.Functor.Align
import X.Functor.Append0
import X.Functor.Fold


class (Map t,Fold t) => TraverseA t where
--  {-# minimal traverse | cocollect | sequence #-}
  traverseA :: Append0 f => (a -> f b) -> t a -> f (t b)
  {-traverse f t = cocollect (\x -> x) (map f t)-}
  {-cocollect :: Append f => (t a -> b) -> t (f a) -> f b-}
  {-cocollect tab tfa = map tab (sequence tfa)-}
  {-sequence :: Append f => t (f a) -> f (t a)-}
  {-sequence = traverse (\x -> x)-}

{-instance TraverseA [] where-}
  {-traverseA = go' where-}
    {-go' f = map (\g -> g []) go where-}
      {-go = \case-}
        {-[] -> \_ -> empty-}
        {-(x:xs) -> _ (:) (f x)  (go xs)-}
