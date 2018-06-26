{-# OPTIONS_GHC -Wno-name-shadowing #-}
module X (
          -- * Basic Types
           Word ,Word8 ,Word16 ,Word32 ,Word64
          ,Int ,Int8, Int16, Int32, Int64
          ,Integer
          ,Char

          -- * Identifiers for @-XRebindableSyntax@
          -- | These identifiers are necessary for [-XRebindableSyntax](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax) to work,
          -- but should generally not be used otherwise
          
          -- ** [Do Notation](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14)
          -- | These are provided by "X.Syntax.Do.X"
         ,return
         ,(>>)
         ,(>>=)
         -- *** With @-XApplicativeDo@
         -- | [-XApplicativeDo](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#applicative-do-notation) will try to be smart about using minmal constraints on do notation.
         -- For example,
         --
         -- > do {x <- fx; y <- fy; return (x,y)}
         -- 
         -- will become
         --
         -- > fmap (,) fx <*> fy
         -- 
         -- and 
         --
         -- > do {x <- fa; return (f x)}
         --
         -- will become simply 
         --
         -- > fmap f fa
         ,fmap,(<*>)
         ) where
import X.Syntax.Do as X (DoSyntax(DoSyntax))
import qualified X.Syntax.Do.X as XDo
import X.Functor.Monad
import X.Type.Char 
import X.Type.Int
import X.Type.Word
import X.Data.Struct.Integer

DoSyntax {..} = XDo.doSyntax
