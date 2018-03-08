module TimesOne (TimesOne,module X) where
import Times as X
import One as X

-- | one * a = a * one = a
class (Times m, One m) => TimesOne m
