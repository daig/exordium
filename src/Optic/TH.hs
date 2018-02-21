{-# language TemplateHaskell #-}
module Optic.TH (mkLenses,mkPrisms,mkOptics) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Map as M
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Data.List as P
import Int.I
import E.Type
import MapM.Class
import FoldMap.Class
import Traversal0.Class

mkLens :: Name -> Q Exp
mkLens label = do
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


mkAffLens :: ([Name],Name) -> [(Name,Int)] -> Q Exp
mkAffLens (constrs,label) otherConstrs = do
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

imap :: (Int -> a -> b) -> [a] -> [b]
imap iab = go 0 where
  go i = \case
    [] -> []
    a:as -> iab i a : go (plus 1 i) as

type Set a = M.Map a ()
{-filterLenses :: M.Map ConstrName Int -> M.Map Label (ConstrName,Int) -> [Label]-}
filterLenses cmap lmap =
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

collectFromList :: (P.Ord k, FoldMap t) => (a -> as) -> (a -> as -> as) -> M.Map k as -> t (k,a) -> M.Map k as
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
{-dataInfo :: DataName -> Q (M.Map ConstrName Int, M.Map Label [(ConstrName,Int)])-}
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
      ForallC{} -> P.error "getConstrs:ForallC"
      GadtC{} -> P.error "getConstrs:GadtC"
      RecGadtC{} -> P.error "getConstrs:RecGadtC"
    varBangType'nameType (name,_,t) = (name,t)

mkPrisms :: Name -> DecsQ
mkPrisms n = do
  (M.assocs -> cmap,_) <- dataInfo n
  let
   cmapMissing l = P.filter (\(l',_) -> l P./= l') cmap
   prismDecl (l,arity) = [d|$(lensName l) = $(mkPrism (l,arity) (cmapMissing l))|]
  P.concat P.<$> P.mapM prismDecl cmap
  

mkLenses :: Name -> DecsQ
mkLenses n = do
  (cmap,lmap) <- dataInfo n
  let
   clist = M.assocs cmap
   clistMissing l = P.filter (\(l',_) -> P.all (P./= l') l) clist
   (labels,affLabels) = filterLenses cmap lmap
   lensDecl l = [d|$(lensName l) = $(mkLens l)|]
   affDecl l =
     let
       constrs = map (\case (c,_) -> c) ( lmap M.! l)
       expr = mkAffLens (constrs,l) (clistMissing constrs)
     in 
       [d|$(lensName l) = $expr|]
       
  lenses <- P.concat P.<$> P.mapM lensDecl labels
  affLenses <- P.concat P.<$> P.mapM affDecl affLabels
  P.pure (lenses P.++ affLenses)

mkOptics :: Name -> DecsQ
mkOptics n = (P.++) P.<$> mkLenses n P.<*> mkPrisms n

lensName :: Name -> PatQ
lensName (Name (OccName o) _) = varP (Name (OccName ('_':o)) NameS)
