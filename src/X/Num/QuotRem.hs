module X.Num.QuotRem (module X.Num.QuotRem, module X) where
import X.Num.FromNatural as X
import X.Num.Recip

-- (x ‘div‘ y) * y + (x ‘mod‘ y) == x
class FromNatural r => QuotRem r where
  quotRem :: r -> r -> (r,r)
  default quotRem :: Recip r => r -> r -> (r,r)
  quotRem a b = let !q = divide a b in (q,zero)
  quot :: r -> r -> r
  quot a b = case quotRem a b of (q,_) -> q
  {-# INLINE quot #-}
  rem :: r -> r -> r
  rem a b = case quotRem a b of (_,r) -> r
  {-# INLINE rem #-}
