{-# OPTIONS_HADDOCK not-home  #-}
{-# language MagicHash #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Num
  (Add(..),Zero(..),Zero'(..),Negate(..)
  ,Mul(..),One(..),One'(..),Recip(..)
  ,Rg,Scale(..)
  ,FromNatural(..),FromInteger(..)
  ,QuotRem(..)
  ,Act(..),Diff(..)
  ,module X
  ) where
import Num.Zero' as X
import Num.Negate as X
import Num.One' as X
import Num.Recip as X
import Num.Scale as X
import Num.Diff as X
import Num.QuotRem as X
import Num.FromInteger as X
