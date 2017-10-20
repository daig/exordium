module AReview (type (|~.), review, module X) where 
import K as X (KK(..)) -- TODO: split out module?
import I as X

type t |~. b = KK b (I b) -> KK t (I t)

review :: t |~. b -> b -> t
review l = \b -> case l (KK (I b)) of KK (I t) -> t
