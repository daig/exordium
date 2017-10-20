module APrism.Market (Market(..), Market', module X) where
import Sum as X (E)
import Sum
import Choice as X
import Dimap as X
import Map as X
import Prelude ((.))

data Market a b s t = Market (b -> t) (s -> E t a)
type Market' a = Market a a

instance Dimap (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (either (L . g) R . seta . f)
  {-# INLINE dimap #-}
  premap f (Market bt seta) = Market bt (seta . f)
  {-# INLINE premap #-}
  postmap f (Market bt seta) = Market (f . bt) (either (L . f) R . seta)
  {-# INLINE postmap #-}
instance ComapL (Market a b) where comapL = premap
instance MapR (Market a b) where mapR = postmap
instance Map (Market a b s) where map = postmap

instance Choice (Market a b) where
  left (Market bt seta) = Market (L . bt) (\case
    L s -> case seta s of
      L t -> L (L t)
      R a -> R a
    R c -> L (R c))
  {-# INLINE left #-}
  right (Market bt seta) = Market (R . bt) (\case
    L c -> L (L c)
    R s -> case seta s of
      L t -> L (R t)
      R a -> R a)
  {-# INLINE right #-}
