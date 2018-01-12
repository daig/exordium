module ApplyF where

import PureF as X

-- TODO: fix name
class PureF t => BindF t where apf :: t m (a -> b) -> t m a -> t m b
