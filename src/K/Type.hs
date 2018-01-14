module K.Type where

newtype K a (b :: *) = K a

newtype KK (a :: *) b = KK b
