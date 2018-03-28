{-# language MagicHash #-}
module X.Prim.IO.STM
  (TVar# ,newTVar#
  ,readTVar#, readTVarIO#
  ,writeTVar#, sameTVar#
  ,atomically#
  ,retry#
  ,catchRetry#
  ,catchSTM#
  ,check#) where
import GHC.Prim
