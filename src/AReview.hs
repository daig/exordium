module AReview (type (|~.), review, module X) where 
import K as X (KK(..)) -- TODO: split out module?

type t |~. b = KK b b -> KK t t

review :: t |~. b -> b -> t
review l = \b -> case l (KK b) of KK t -> t
