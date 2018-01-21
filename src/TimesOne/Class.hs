module TimesOne.Class (TimesOne(..),module X) where
import Times.Class as X
import One.Class as X

-- | one * a = a * one = a
class (Times m, One m) => TimesOne m
