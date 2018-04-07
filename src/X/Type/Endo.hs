module X.Type.Endo where
import X.Arrow.Folded
import X.Arrow.Precoerce
import X.Functor.Comap
import X.Arrow.Propure
import X.Functor.Append0

newtype Endo p a = Endo {runEndo :: p a a}
instance Folded_ p => Comap (Endo p) where
  comap f (Endo p) = Endo (folding_ f p)
{-instance Precoerce p => Map (Endo p) where-}
  {-map f (Endo p) = Endo (from f p)-}
{-instance Promap p => Remap (Endo p) where-}
  {-remap f g (Endo p) = Endo (promap f g p)-}
{-{-instance (Promap p, Compose p) => FPlus (Endo p) where-}-}
  {-{-fplus (Endo p) (Endo q) = Endo (-}-}
{-instance Identity p => Empty (Endo p) where empty = Endo identity-}
{-{-instance Category p => Append0 (Endo p)-}-}
{-instance (Precoerce p, Propure p) => Pure (Endo p) where pure a = Endo (propure (\_ -> a))-}


-- 1 ~ Empty
-- E ~ Top
-- X ~ Pure
--
{-data Box f g :: * -> * where-}
  {-Box :: (a -> x) -> (a -> y) -> (x -> y -> a) -> f x -> g y -> Box f g a-}
{-data Couchy f g :: * -> * where-}
  {-Couchy :: (a -> E x y) -> (x -> a) -> (y -> a) -> f x -> g y -> Couchy f g a-}

{-instance Remap (Couchy f g) where-}
  {-remap ba ab (Couchy axy xa ya fx gy)-}
    {-= Couchy (\(ba -> a) -> axy a) (\(xa -> a) -> ab a) (\(ya -> a) -> ab a) fx gy-}

{-data FCouchy f g :: * -> * where-}
  {-FCouchy :: (x -> a) -> (y -> a) -> f x -> g y -> FCouchy f g a-}
{-instance Remap (FCouchy f g) where remap _ = map-}
{-instance Map (FCouchy f g) where-}
  {-map ab (FCouchy xa ya fx gy)-}
    {-= FCouchy (\(xa -> a) -> ab a) (\(ya -> a) -> ab a) fx gy-}

{-data CCouchy f g :: * -> * where-}
  {-CCouchy :: (a -> E x y) -> f x -> g y -> CCouchy f g a-}

{-instance Remap (CCouchy f g) where remap ba _ = comap ba-}
{-instance Comap (CCouchy f g) where-}
  {-comap ba (CCouchy axy fx gy)-}
    {-= CCouchy (\(ba -> a) -> axy a) fx gy-}

-- | Couchy f f a -> f a
{-class Remap f => FPlus f => f a -> f b -> f (E a b)-}

{-data FDay c f g :: * -> * where-}
  {-(c x y -> a) -> f x -> g y -> Day c f g (-}
class Remap f => Costrong f where
  {-# minimal costrong | costrengthen #-}
  costrong :: f (a,b) -> (a,f b)
  costrong = costrengthen (,) (\(a,_) -> a) (\(_,b) -> b)
  costrengthen :: (a -> b -> ab) -> (ab -> a) -> (ab -> b) -> f ab -> (a, f b)
  costrengthen f l r fab = costrong (remap (\(x,y) -> f x y) (\xy -> (l xy, r xy)) fab)
  split :: f a -> (a, f ())
  split = costrengthen (\a _ -> a) (\a -> a) (\_ -> ())
  extract :: f a -> a
  extract (split -> (a,_)) = a
  shape :: f a -> f ()
  shape (split -> (_,fx)) = fx
  

class Remap f => Choice f where
  {-# minimal choice | rechoose #-}
  choice :: f (E a b) -> E a (f b)
  choice = rechoose L R (\e -> e)
  rechoose :: (a -> c) -> (b -> c) -> (c -> E a b) -> f c -> E a (f b)
  rechoose l r c f = choice (remap (\case {L a -> l a; R b -> r b}) c f)
  extract' :: f a -> E a (f X)
  extract' = rechoose (\a -> a) (\case {}) L

class (Choice f, Map f) => FChoice f where
  choose :: (c -> E a b) -> f c -> E a (f b)
  choose c f = choice (map c f)

class (Choice f, Comap f) => CChoice f where
  cchoose :: (a -> c) -> (b -> c) -> f c -> E a (f b)
  cchoose l r f = choice (comap (\case {L a -> l a; R b -> r b}) f)

class Remap f => Cochoice f where
  cochoice :: E a (f b) -> f (E a b)
  cochoice = cochoose (\e -> e) L R
  cochoose :: (c -> E a b) -> (a -> c) -> (b -> c) -> E a (f b) -> f c
  cochoose f l r a = remap f (\case {L x -> l x; R y -> r y}) (cochoice a)
  pure' :: E a (f X) -> f a
  pure' = cochoose L (\a -> a) (\case {})
  pure :: a -> f a
  pure a = pure' (L a)
  absurd :: f X -> f a
  absurd fx = pure' (R fx)
