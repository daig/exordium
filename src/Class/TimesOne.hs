module Class.TimesOne (TimesOne(..),module X) where
import Class.Times as X
import Class.One as X

-- | one * a = a * one = a
class (Times m, One m) => TimesOne m
