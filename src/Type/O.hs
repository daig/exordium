module Type.O where

newtype O f g a = O (f (g a))

{-instance (Pure f,Pure g) => Pure (O f g) where pure a = O (pure (pure a))-}
{-instance (Apply f,Apply g) => Apply (O f g) where O fgf |$| O fga = O (fgf |$(|$|)$| fga)-}
{-instance (Applicative f, Applicative g) => Applicative (O f g)-}
