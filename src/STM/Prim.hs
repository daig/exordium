module STM.Prim (module X) where
import GHC.Prim as X
  (TVar#, State#
  ,atomically#, retry#
  ,catchRetry#, catchSTM#
  ,check#
  ,newTVar# ,readTVar#, readTVarIO#, writeTVar#
  ,sameTVar#)
