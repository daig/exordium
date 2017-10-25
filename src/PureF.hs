module PureF where

import MapF as X

class MapF t => PureF t where puref :: MapFC t f => f a -> t f a
