{-# OPTIONS_GHC -Wno-orphans #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
module IO where
import Type.IO as X
{-import Traverse_ as X-}
{-import Class.Monad-}

{-instance MapIso IO where mapIso = map_mapIso-}
{-instance Map IO where-}
  {-map f (IO k) = IO (\s -> case k s of {(# s', a #) -> (# s', f a #)})-}
  {-b !@ IO k = IO (\s -> case k s of {(# s', _ #) -> (# s', b #)})-}
{-instance Bind IO where join (IO k) = IO (\s -> case k s of {(# s', IO k' #) -> k' s'})-}
{-instance Apply IO where (|$|) = apDefault-}
{-instance Pure IO where pure a = IO (\s -> (# s , a #))-}

{-instance Traverse_ IO where-}
  {-traverse_ f (IO io) = IO (\s -> -}
