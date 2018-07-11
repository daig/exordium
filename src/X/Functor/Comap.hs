-- {-# language BlockArguments #-}
module X.Functor.Comap (Comap(..)) where
import X.Type.K
import X.Type.Permute
import X.Ops.Fun
import X.Cast.Coerce

import qualified Prelude as P

class Comap f where comap :: (b -> a) -> f a -> f b

instance Comap (K a) where comap _ (K a) = K a
instance Comap (BA (->) a) where comap ba (BA ar) = BA (ba > ar)

{-data CoList a = Done (P.IO ()) | More (a -> CoList a)-}
{-instance Comap CoList where-}
  {-comap ba = \case -}
    {-Done io -> Done io-}
    {-More al -> More \b -> comap ba (al (ba b))-}
{-feed :: CoList a -> [a] -> P.IO ()-}
{-feed (Done io) _ = io-}
{-feed (More al) (a:as) = feed (al a) as-}
{-feed _ [] = P.putStrLn "Not enough arguments!"-}

{-test = More \a -> More \b -> More \c -> Done (P.putStrLn (a P.++ b P.++ c))-}
{-test' = More \a -> More \b -> Done (P.putStrLn (P.reverse a P.++ P.reverse b))-}
{-test2 = div (P.splitAt 3) test test'-}
{-class Comap f => Div f where-}
  {-div :: (a -> (b,c)) -> f b -> f c -> f a-}
  {-split :: f a -> f a -> f a-}
  {-conq :: f a-}
{-instance Div CoList where-}
  {-div abc (More bl) (More cl)  = More \(abc -> (b,c)) -> div abc (bl b) (cl c)-}
  {-div abc l (More cl)          = More \(abc -> (b,c)) -> div abc l (cl c)-}
  {-div abc (More bl) r          = More \(abc -> (b,c)) -> div abc (bl b) r-}
  {-div abc (Done io) (Done io') = Done (io P.>> io')-}
  {-conq = Done (P.return ())-}
{-class COT t where cot :: Div f => t (f a) -> f (t a)-}
{-instance COT CoList where-}
  {-cot = \case-}
    {-Done{} -> conq-}
    {-l@(More (al :: f a -> CoList (f a)) -> div (\(l :: CoList a) -> -}
