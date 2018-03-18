{-# language TemplateHaskell #-}
module Derive.Optic (mkTraverseds_,mkPrisms,mkOptics) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Data.List as P
import Functor.Plus
import Type.Int.I
import Functor.Map'
import Functor.IMap
import Functor.Fold
import Arrow.Traversed
import Debug.Trace

mkTraversed_ :: Name -> Q Exp
mkTraversed_ label = do
  s <- newName "s"
  b <- newName "b"
  P.pure P.$
    VarE 'lens `AppE` VarE label `AppE`
       (LamE [VarP s, VarP b]
             (RecUpdE (VarE s) [(label,VarE b)]))

mkPrism :: (Name,Int) -> [(Name,Int)] -> Q Exp
mkPrism (constr,arity) otherConstrs = do
  passMatches <- P.mapM (\(n,i) -> passMatch n i) otherConstrs
  (match,con) <- if arity P.== 1 then (,) P.<$> match1 P.<*> con1
                                 else (,) P.<$> matchN P.<*> conN
  P.pure P.$
    VarE 'prism `AppE` 
      LamCaseE (match
               :passMatches)
      `AppE` con
  where
    con1 = P.pure P.$ ConE constr
    conN = do
      args <- P.replicateM arity (newName "x") 
      P.pure P.$ LamCaseE
       [Match (TupP (map VarP args))
              (NormalB (P.foldl (\e a -> AppE e (VarE a)) (ConE constr) args))
              []]
      
    match1 = do
      x <- newName "x"
      P.pure P.$ Match (ConP constr [VarP x]) (NormalB (AppE (ConE 'R) (VarE x))) []
    matchN = do
      args <- P.replicateM arity (newName "x") 
      P.pure P.$
        Match (ConP constr (map VarP args)) (NormalB (ConE 'R `AppE` TupE (map VarE args))) []

passMatch :: Name -> Int -> Q Match
passMatch constr arity = do
  args <- P.replicateM arity (newName "x") 
  P.pure P.$ Match (constr `ConP` map VarP args)
                   (NormalB (ConE 'L `AppE` P.foldl (\e a -> AppE e (VarE a)) (ConE constr) args))
                   []


mkAffTraversed_ :: ([Name],Name) -> [(Name,Int)] -> Q Exp
mkAffTraversed_ (constrs,label) otherConstrs = do
  passMatches <- P.mapM (\(n,i) -> passMatch n i) otherConstrs
  s <- newName "s"
  t <- newName "t"
  a <- newName "a"
  b <- newName "b"
  let
   match constr =
    Match (RecP constr [(label,VarP a)])
          (NormalB (ConE 'R `AppE` VarE a))
          []
   matches = map match constrs
  P.pure P.$
    VarE 'lens0 `AppE` 
    (LamCaseE (matches P.++ passMatches)) `AppE`
    (LamE [VarP s, VarP b]
          (RecUpdE (VarE s) [(label,VarE b)]))
      
-- need to be clever and case match on every constructor for full polymorphism
{-pp = lens0 (\case {s@A{} -> R (wow s); B b -> L (B b)})-}
           {-(\s b -> case s of {s'@A{} -> s' {wow = b}; B b -> B b})-}


{-getLabel1 name = P.fmap (\(P.head -> (_,P.head -> Just n)) -> n) (getConstrs name)-}
{-getLabel2 name = P.fmap (\(P.head -> (_,(_:Just n:_))) -> n) (getConstrs name)-}

type Set a = M.Map a ()
{-filterTraverseds_ :: M.Map ConstrName Int -> M.Map Label (ConstrName,Int) -> [Label]-}
filterTraverseds_ cmap lmap =
  let
    labels = M.keys lmap
    cmap'size = M.size cmap
    labels' = P.partition (\l -> P.length (lmap M.! l ) P.== cmap'size) labels
    {-labelConstrArity l = case lmap M.! l of (c,_) -> cmap M.! c-}
  in
    if cmap'size P.== 1
      then (labels,[])
      else labels'
    {-filter (\label -> -}

collectFromList :: (P.Ord k, Fold t) => (a -> as) -> (a -> as -> as) -> M.Map k as -> t (k,a) -> M.Map k as
collectFromList f0 f m0 x = foldr (\(k,a) -> M.alter (f' a) k) m0 x where
  f' a' = \case
    P.Nothing -> P.Just (f0 a')
    P.Just as -> P.Just (f a' as)
    
mapMaybe :: (a -> P.Maybe b) -> [a] -> [b]
mapMaybe f = go where
  go = \case
    [] -> [] 
    a:as -> case f a of
      P.Nothing -> go as
      P.Just b -> b:go as

type DataName = Name
type ConstrName = Name
type Label = Name
dataInfo :: DataName -> Q (M.Map ConstrName Int, M.Map Label [(ConstrName,Int)])
dataInfo n = do
  TyConI (DataD _cxt _name _tyVarsBndrs _kind' (map con'args -> conargs) _deriv) <- reify n
  let
    constrMap = M.fromList (map (\(c,args) -> (c,P.length args)) conargs)
    labelMap = collectFromList P.pure (:) M.empty
           P.$ mapMaybe (\(arg,con) -> P.fmap (,con) arg)
           P.$ (P.concatMap labelInfo conargs :: [(P.Maybe Name, (Name, Int))])
  P.pure (constrMap,labelMap)
  where
    labelInfo :: (Name,[P.Maybe Name]) -> [(P.Maybe Name,(Name,Int))]
    labelInfo (con,args) = imap (\i lab -> (lab,(con,i))) args
    name'unpack (Name oc flav,_,_) = (oc,flav)
    con'args :: Con -> (Name,[P.Maybe Name])
    con'args = \case
      NormalC con args -> (con, P.Nothing P.<$ args)
      RecC con labs -> (con,map (\(label,_,_) -> P.Just label) labs)
      InfixC _ con _ -> (con, [P.Nothing,P.Nothing])
      ForallC _tyvarbndrs _cxt c -> con'args c -- P.error "getConstrs:ForallC"
      GadtC cs args _type -> case cs of
        [con] -> con'args (NormalC con args)
        _ -> P.error ("getConstrs:GadtC expecting one constructor but got " `fplus` P.show cs)
      RecGadtC cs labs _type -> case cs of
        [con] -> con'args (RecC con labs)
        _ -> P.error ("getConstrs:RecGadtC expecting one constructor but got " `fplus` P.show cs)
    varBangType'nameType (name,_,t) = (name,t)

mkPrisms :: Name -> DecsQ
mkPrisms n = do
  (M.assocs -> cmap,_) <- dataInfo n
  let
   cmapMissing l = P.filter (\(l',_) -> l P./= l') cmap
   prismDecl (l,arity) = [d|$(lensName l) = $(mkPrism (l,arity) (cmapMissing l))|]
  P.concat P.<$> P.mapM prismDecl cmap
  

mkTraverseds_ :: Name -> DecsQ
mkTraverseds_ n = do
  (cmap,lmap) <- dataInfo n
  let
   clist = M.assocs cmap
   clistMissing l = P.filter (\(l',_) -> P.all (P./= l') l) clist
   (labels,affLabels) = filterTraverseds_ cmap lmap
   lensDecl l = [d|$(lensName l) = $(mkTraversed_ l)|]
   affDecl l =
     let
       constrs = map (\case (c,_) -> c) ( lmap M.! l)
       expr = mkAffTraversed_ (constrs,l) (clistMissing constrs)
     in 
       [d|$(lensName l) = $expr|]
       
  lenses <- P.concat P.<$> P.mapM lensDecl labels
  affTraverseds_ <- P.concat P.<$> P.mapM affDecl affLabels
  P.pure (lenses P.++ affTraverseds_)

mkOptics :: Name -> DecsQ
mkOptics n = (P.++) P.<$> mkTraverseds_ n P.<*> mkPrisms n

lensName :: Name -> PatQ
lensName (Name (OccName o) _) = varP (Name (OccName ('_':o)) NameS)
