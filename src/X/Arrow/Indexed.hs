{-# language UndecidableSuperClasses #-}
module X.Arrow.Indexed (module X.Arrow.Indexed,module X) where
import X.Arrow.Folded
import X.Functor.Coerce1
import X.Arrow.Closed
{-import X.Arrow.Representable-}
import X.Arrow.Sieve
import X.Optic.Traversing as X
import X.Arrow.Tabulated

import X.Type.Int

class (Promap p, PIndexed i q q) => PIndexed i q p | p -> q where
  pix :: p a b -> i -> q a b
  default pix :: q ~ p => p a b -> i -> q a b
  pix p _ = p

reindexed :: PIndexed j q p => (i -> j) -> ((i -> q a b) -> r) -> p a b -> r
reindexed ij iqabr pab = iqabr (\i -> pix pab (ij i))

newtype Indexingg f a b = Indexingg {runIndexingg :: a -> Int -> (Int, f b)}
instance Map f => Promap (Indexingg f) where
  promap f g (Indexingg aiifb) = Indexingg \(f -> a) i -> map g `_2` aiifb a i
instance Map f => Traversed_ (Indexingg f) where
  lens sa sbt (Indexingg aiifb) = Indexingg \s i -> _2 (map (sbt s)) (aiifb (sa s) i)
instance Apply f => Traversed1 (Indexingg f) where
  traversal1 afbsft (Indexingg aiifb) = Indexingg \s i -> (i,afbsft (\a -> snd (aiifb a i)) s)
instance Pure f => Traversed' (Indexingg f) where
  prism seta bt (Indexingg aiifb) = Indexingg \s i -> case seta s of
    L t -> (i,pure t)
    R a -> _2 (map bt) (aiifb a i)
instance Pure f => Traversed0 (Indexingg f) where
  traversal0 afbsft (Indexingg aiifb) = Indexingg \s i -> (i,afbsft (\a -> snd (aiifb a i)) s)
instance Applicative f => Traversed (Indexingg f) where
  traversal afbsft (Indexingg aiifb) = Indexingg \s i -> unFIndex (afbsft (\a -> uncurry FIndex (aiifb a i)) s)

snd (_,b) = b
curry :: ((a,b) -> r) -> a -> b -> r
curry f a b = f (a,b)
uncurry :: (a -> b -> r) -> (a,b) -> r
uncurry abr (a,b) = abr a b

indexing :: (PIndexed Int q p,Tabulated q)
         => (Indexingg (Rep q) a b -> Indexingg (Rep q) s t)
         -> p a b -> q s t
indexing l pab = tabulateP \s -> case l (Indexingg go) of
  Indexingg siift -> case siift s 0 of (_,ft) -> ft
  where go a !i = (add 1 i,sieve (pix pab i) a)

unFIndex (FIndex i fa) = (i,fa)
data FIndex f a = FIndex {-# unpack #-} !Int !(f a)
instance Map f => Map (FIndex f) where map f (FIndex i fa) = FIndex i (map f fa)
instance Remap f => Remap (FIndex f) where
  remap f g (FIndex i fa) = FIndex i (remap f g fa)
instance Strong f => Strong (FIndex f) where strong a (FIndex i fb) = FIndex i (strong a fb)
instance Pure f => Pure (FIndex f) where pure a = FIndex 1 (pure a)
instance Apply f => Apply (FIndex f) where ap (FIndex i f) (FIndex j g) = FIndex (add i j) (ap f g)
instance Apply f => FTimes (FIndex f) where ftimes fa fb = ap (map (,) fa) fb
instance Applicative f => Applicative (FIndex f)

{-indexing l iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (add 1 i, pix iafb i a))) s) 0-}
{-indexing :: PIndexed Int q p => (Indexing q a b -> Indexing q s t) -> p a b -> q s t-}
{-indexing l pab = case l (Indexing go) of-}
  {-Indexing ipaib -> postmap (\(_,t) -> t) (ipaib 0)-}
  {-where go !i = (\x -> (add 1 i,x)) `postmap` pix pab i-}
{-indexingF :: PIndexed Int q p => (Indexing' q a b -> Indexing' q s t) -> p a b -> q s t-}
{-indexingF l pab = case l (Indexing' go) of-}
  {-Indexing' ipaib -> (\(_,p) -> p) (ipaib 0)-}
  {-where go !i = (add 1 i, pix pab i)-}

  {-

newtype IndexingH p a b = IndexingH (p (Int,a) (Int,b))
instance Promap p => Promap (IndexingH p) where
  promap f g (IndexingH piaib) = IndexingH (promap (_2 f) (_2 g) piaib)
instance Traversed_ p => Traversed_ (IndexingH p) where
  traversal_ afbsft (IndexingH piaib) = IndexingH
    (traversal_ (\iafib (i,s) -> unFIndex (afbsft (\a -> FIndex (iafib (i,a))) s)) piaib)
instance Traversed1 p => Traversed1 (IndexingH p) where
  traversal1 afbsft (IndexingH piaib) = IndexingH
    (traversal1 (\iafib (i,s) -> unFIndex (afbsft (\a -> FIndex (iafib (i,a))) s)) piaib)
instance Traversed' p => Traversed' (IndexingH p) where
  prism seta bt (IndexingH piaib) = IndexingH (prism (\(i,s) -> case seta s of L t -> L (i,t)
                                                                               R a -> R (i,a))
                                                     (_2 bt)
                                                     piaib)
instance Traversed0 p => Traversed0 (IndexingH p) where
  traversal0 afbsft (IndexingH piaib) = IndexingH
    (traversal0 (\iafib (i,s) -> unFIndex (afbsft (\a -> FIndex (iafib (i,a))) s)) piaib)
instance Traversed p => Traversed (IndexingH p) where
  traversal afbsft (IndexingH piaib) = IndexingH
    (traversal (\iafib (i,s) -> unFIndex (afbsft (\a -> FIndex (iafib (i,a))) s)) piaib)
     

newtype FIndex f a = FIndex {unFIndex :: f (Int,a)}
instance Map f => Map (FIndex f) where map f (FIndex fia) = FIndex (map (_2 f) fia)
instance Remap f => Remap (FIndex f) where remap f g (FIndex fia) = FIndex (remap (_2 f) (_2 g) fia)
instance Map f => Strong (FIndex f) where
  strong a (FIndex fib) = FIndex (map (\(a',(i,b)) -> (i,(a',b))) (strong a fib))
instance Pure f => Pure (FIndex f) where pure a = FIndex (map (1,) (pure a))
instance Apply f => Apply (FIndex f) where
  ap (FIndex fif) (FIndex fja) = FIndex (ap (map (\(i,f) (j,a) -> (add i j, f a)) fif) fja)
instance Apply f => FTimes (FIndex f) where
  ftimes fia fib = ap (map (,) fia) fib
instance Applicative f => Applicative (FIndex f)
 
 -}

  
  {-\i -> traversal_ afbsft `_2` iipab i-}

{-newtype Indexing' p a b = Indexing' {runIndexing' :: Int -> (Int,p a b)}-}
{-instance Promap p => Promap (Indexing' p) where-}
  {-promap f g (Indexing' iipab) = Indexing' \i -> promap f g `_2` iipab i-}
{-instance Traversed_ p => Traversed_ (Indexing' p) where-}
  {-traversal_ afbsft (Indexing' iipab) = Indexing' \i -> traversal_ afbsft `_2` iipab i-}
{-instance Traversed1 p => Traversed1 (Indexing' p) where-}
  {-traversal1 afbsft (Indexing' iipab) = Indexing' \i -> traversal1 afbsft `_2` iipab i-}
{-instance Traversed' p => Traversed' (Indexing' p) where-}
  {-prism seta bt (Indexing' iipab) = Indexing' \i -> prism seta bt `_2` iipab i-}
{-instance Traversed0 p => Traversed0 (Indexing' p) where-}
  {-traversal0 afbsft (Indexing' iipab) = Indexing' \i -> traversal0 afbsft `_2` iipab i-}
{-instance Traversed p => Traversed (Indexing' p) where-}
  {-traversal afbsft (Indexing' iipab) = Indexing' \i -> traversal afbsft `_2` iipab i-}
  

newtype Indexing p a b = Indexing {runIndexing :: Int -> p a (Int, b)}
instance Promap p => Promap (Indexing p) where
  promap f g (Indexing ipaib) = Indexing \i -> promap f (\(j,b) -> (j,g b)) (ipaib i)
instance Traversed_ p => Traversed_ (Indexing p) where
  {-lens sa sbt (Indexing ipaib) = Indexing \i -> lens sa (\s (j,b) -> (j,sbt s b)) (ipaib i)-}
  traversed_ (Indexing ipaib) = Indexing \i -> (\t -> (i,map (\(_,b) -> b) t))  `postmap` traversed_ (ipaib i)
  {-traversal_ afbsft (Indexing ipaib) = Indexing \i -> traversal_ go (ipaib i) where-}
    {-go afib s = map (0,) (afbsft (\a -> snd `map` afib a) s)-}
    {-snd (_,b) = b-}

{-traversal_ afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-newtype Indexing f a = Indexing {runIndexing :: Int -> (Int, f a)}-}
instance Traversed' p => Traversed' (Indexing p) where
  prism seta bt (Indexing ipaib) = Indexing \i -> postmap (i,) (prism seta
                                                                      (\(_,b) -> bt b) 
                                                                      (ipaib i))


instance Traversed1 p => Traversed1 (Indexing p) where
  traversed1 (Indexing ipaib) =
    Indexing \i -> (\t -> (i,map (\(_,b) -> b) t))  `postmap` traversed1 (ipaib i)

instance Traversed0 p => Traversed0 (Indexing p) where
  traversed0 (Indexing ipaib) =
    Indexing \i -> (\t -> (i,map (\(_,b) -> b) t))  `postmap` traversed0 (ipaib i)

instance Traversed p => Traversed (Indexing p) where
  traversed (Indexing ipaib) =
    Indexing \i -> (\t -> (i,map (\(_,b) -> b) t))  `postmap` traversed (ipaib i)



  
newtype IndexingT i p a b = IndexingT {runIndexingT :: i -> p a b}
instance Promap p => Promap (IndexingT i p) where promap f g (IndexingT ipab) = IndexingT (map (promap f g) ipab)
instance Traversed_ p => Traversed_ (IndexingT i p) where lens sa sbt (IndexingT ipab) = IndexingT (map (lens sa sbt) ipab)
instance (Promap p, PIndexed i p p) => PIndexed i p (IndexingT i p) where pix = runIndexingT

{-newtype ITraversing i f a b = ITraversing {runITraversing :: i -> a -> f b}-}
{-_ITraversing :: Promap p => p (ITraversing i f a b) (Traversing f s t) -> p (i -> a -> f b) (s -> f t)-}
{-_ITraversing = promap ITraversing runTraversing-}



{-instance Zip f => Closed (ITraversing i f) where-}
  {-closed (ITraversing iafb) = ITraversing (\i xa -> distribute (\x -> iafb i (xa x)))-}
{-instance Map f => Promap (ITraversing i f) where promap f g (ITraversing s) = ITraversing (\i -> (promap f (map g) (s i)))-}
{-instance Strong f => Strong (ITraversing i f a) where-}
  {-strong x (ITraversing iafb) = ITraversing (\i a -> strong x (iafb i a))-}
{-instance Map f => Map (ITraversing i f a) where-}
  {-map f (ITraversing s) = ITraversing (\i a -> map f (s i a))-}
{-instance Remap f => Remap (ITraversing i f a) where-}
  {-remap f g (ITraversing s) = ITraversing (\i a -> remap f g (s i a))-}
{-{--- TODO: move to PromapIso class-}-}
{-instance Comap f => Comap (ITraversing i f a) where-}
  {-comap f (ITraversing s) = ITraversing (\i a -> comap f (s i a))-}
{-instance Coerce1 f => Folded_ (ITraversing i f) where-}
   {-postcoerce (ITraversing s) = ITraversing (\i a -> coerce1 (s i a))-}

{-instance Map f => Traversed_ (ITraversing i f) where-}
  {-traversal_ afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-instance Pure f => Traversed' (ITraversing i f) where-}
  {-prism pat constr (ITraversing iafb) = ITraversing (\i s -> case pat s of-}
    {-L t -> pure t-}
    {-R a -> constr `map` iafb i a)-}
{-instance Apply f => Traversed1 (ITraversing i f) where-}
  {-traversal1 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-instance Pure f => Traversed0 (ITraversing i f) where-}
  {-traversal0 afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}
{-instance Applicative f => Traversed (ITraversing i f) where-}
  {-traversal afbsft (ITraversing iafb) = ITraversing (\i s -> afbsft (iafb i) s)-}

instance PIndexed i (->) (->)

