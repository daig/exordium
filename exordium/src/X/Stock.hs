{-# OPTIONS_HADDOCK show-extensions #-}
--------------------------------------------------------------------
-- |
-- Module    :  Stock 
-- Copyright :  (c) Dai 2018
-- License   :  MIT
-- Maintainer:  dailectic@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
-- Stock classes which ghc may cleverly derive. Instances should not be provided manually.
-- Meant to be imported qualified.
--------------------------------------------------------------------
module X.Stock
  (Stock(..)
  ,Eq(..)
  ,Num(..), Integer
  ,Ord(..),Ordering(..), Bool
  ,Functor(..)
  ,Foldable(..)
  {-,Lift(..)-}
  ,Traversable(..)
  ,Enum(..)
  ,Bounded(..)
  ,Read(..)
  ,Show(..)
  ) where
import GHC.Classes
import GHC.Show
import GHC.Read
import GHC.Enum
import GHC.Base
import GHC.Num
import Data.Foldable
import Data.Traversable


-- | A wrapper for providing stock instances
newtype Stock a = Stock {unStock :: a}
