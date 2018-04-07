module X.Functor.FTop where
import X.Functor.Comap as X
import X.Data.X
import X.Data.E
import X.Functor.Map as X
import X.Functor.Align 
import X.Data.These as X
import qualified Prelude as P

class Remap f => FTop f where ftop :: (f a -> a) -> f a
class Remap f => FBot f where fbot :: (a -> f a) -> f a

instance FTop [] where
  ftop faa = go 0 [] where
    {-go n as = let as' = faa (P.take n as) : go as' in as'-}
    go n as = let as' = faa (P.take n as) : go (n P.+ 1) as' in as'

class Remap f => FMin f where fmin :: f a -> f b -> f (a,b)

class Remap f => FMax f where fmax :: f a -> f b -> f (These a b)


{-class FWith f => (x -> These a b) -> (a -> x) -> (b -> x) -> (a -> b -> x) -> f a -> f b -> f x-}


class Remap f => FTimes f where ftimes :: f a -> f b -> f (a,b)
{-class (FTimes f,Map f) => ftimesWith :: (a -> b -> c) -> f a -> f b -> f c-}


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

{--- | Couchy f f a -> f a-}
{-{-class Remap f => FPlus f => f a -> f b -> f (E a b)-}-}

{-{-data FDay c f g :: * -> * where-}-}
  {-{-(c x y -> a) -> f x -> g y -> Day c f g (-}-}
{-class Remap f => Strong f where-}
  {-{-# minimal strong | strengthen #-}-}
  {-strong :: a -> f b -> f (a,b)-}
  {-strong = strengthen (\(a,_) -> a) (\(_,b) -> b) (,)-}
  {-strengthen :: (c -> a) -> (c -> b) -> (a -> b -> c) -> a -> f b -> f c-}
  {-strengthen f g h a fb = remap (\c -> (f c, g c)) (\(x,y) -> h x y) (strong a fb)-}
  {-fill :: a -> f () -> f a-}
  {-fill = strengthen (\a -> a) (\_ -> ()) (\a _ -> a)-}
{-class Remap f => Costrong f where-}
  {-{-# minimal costrong | costrengthen #-}-}
  {-costrong :: f (a,b) -> (a,f b)-}
  {-costrong = costrengthen (,) (\(a,_) -> a) (\(_,b) -> b)-}
  {-costrengthen :: (a -> b -> ab) -> (ab -> a) -> (ab -> b) -> f ab -> (a, f b)-}
  {-costrengthen f l r fab = costrong (remap (\(x,y) -> f x y) (\xy -> (l xy, r xy)) fab)-}
  {-split :: f a -> (a, f ())-}
  {-split = costrengthen (\a _ -> a) (\a -> a) (\_ -> ())-}
  {-extract :: f a -> a-}
  {-extract (split -> (a,_)) = a-}
  {-shape :: f a -> f ()-}
  {-shape (split -> (_,fx)) = fx-}
  

{-class Remap f => Choice f where-}
  {-{-# minimal choice | rechoose #-}-}
  {-choice :: f (E a b) -> E a (f b)-}
  {-choice = rechoose L R (\e -> e)-}
  {-rechoose :: (a -> c) -> (b -> c) -> (c -> E a b) -> f c -> E a (f b)-}
  {-rechoose l r c f = choice (remap (\case {L a -> l a; R b -> r b}) c f)-}
  {-extract' :: f a -> E a (f X)-}
  {-extract' = rechoose (\a -> a) (\case {}) L-}

{-class (Choice f, Map f) => FChoice f where-}
  {-choose :: (c -> E a b) -> f c -> E a (f b)-}
  {-choose c f = choice (map c f)-}

{-class (Choice f, Comap f) => CChoice f where-}
  {-cchoose :: (a -> c) -> (b -> c) -> f c -> E a (f b)-}
  {-cchoose l r f = choice (comap (\case {L a -> l a; R b -> r b}) f)-}

{-class Remap f => Cochoice f where-}
  {-cochoice :: E a (f b) -> f (E a b)-}
  {-cochoice = cochoose (\e -> e) L R-}
  {-cochoose :: (c -> E a b) -> (a -> c) -> (b -> c) -> E a (f b) -> f c-}
  {-cochoose f l r a = remap f (\case {L x -> l x; R y -> r y}) (cochoice a)-}
  {-pure' :: E a (f X) -> f a-}
  {-pure' = cochoose L (\a -> a) (\case {})-}
  {-pure :: a -> f a-}
  {-pure a = pure' (L a)-}
  {-absurd :: f X -> f a-}
  {-absurd fx = pure' (R fx)-}
