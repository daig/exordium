{-# OPTIONS_HADDOCK not-home  #-}
{-# language MagicHash #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module X.Num
  (Add(..),Zero(..),Zero'(..),Negate(..)
  ,Mul(..),One(..),One'(..),Recip(..)
  ,Rg,Scale(..)
  ,FromNatural(..),FromInteger(..)
  ,QuotRem(..)
  ,Act(..),Diff(..)
  ) where
import X.Num.Zero' as X
import X.Num.Negate as X
import X.Num.One' as X
import X.Num.Recip as X
import X.Num.Scale as X
import X.Num.Diff as X
import X.Num.QuotRem as X
import X.Num.FromInteger as X
