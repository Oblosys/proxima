

-- UUAGC 0.9.10 (Transform.ag)
module Transform where

import Control.Monad(mplus,mzero)
import List (partition, elem, nub,intersperse)
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set as Set (Set, member, union, toList, fromList, empty, singleton, member, unions, size, fold, intersection, difference, insert)
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, empty, (><),fromList)
import Data.Foldable(toList)
import UU.Scanner.Position(noPos)

import ConcreteSyntax
import AbstractSyntax
import ErrorMessages
import Patterns (Patterns(..),Pattern(..))
import Expression (Expression(..))
import HsToken

import Options
import CommonTypes


import UU.Scanner.Position (Pos)
import Patterns   (Pattern)
import Expression (Expression)
import CommonTypes


-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)
type DefinedSets = Map Identifier (Set NontermIdent) 

type FieldMap  = [(Identifier, Type)] 

type DataTypes = Map.Map NontermIdent (Map.Map ConstructorIdent FieldMap) 

type AttrName   = (Identifier,Identifier) 

type RuleInfo   = ([AttrName]->Pattern, Expression, [AttrName], Bool, String) 

type SigInfo    = (Identifier,Type) 

type UniqueInfo = (Identifier,Identifier) 



checkDuplicate :: (Identifier -> Identifier -> Error)
               -> Identifier -> val -> Map Identifier val -> (Map Identifier val,Seq Error)
checkDuplicate dupError key val m
  = case Map.lookupIndex key m of
     Just ix -> let (key',_) = Map.elemAt ix m
                in  (m,Seq.singleton (dupError key key'))
     Nothing -> (Map.insert key val m,Seq.empty)

checkDuplicates :: (Identifier -> Identifier -> Error)
                -> [(Identifier, val)] -> Map Identifier val -> (Map Identifier val,Seq Error)
checkDuplicates dupError new m = foldErrors check m new
 where  check = uncurry (checkDuplicate dupError)

foldErrors f e xs = foldl g (e,Seq.empty) xs
  where g ~(e,es) x = let (e',es') = f x e
                      in (e', es >< es')


checkForDuplicates :: (Identifier -> Identifier -> Error)  ->  [Identifier]  ->  [Error]
checkForDuplicates err [] = []
checkForDuplicates err (x:xs) = let (same,other) = List.partition (equalId x) xs
                                in  map (err x) same ++ checkForDuplicates err other

equalId :: Identifier -> Identifier -> Bool
equalId x y = getName x == getName y



type RulesAndErrors = ([Rule], Seq Error)
type SigsAndErrors  = ([TypeSig], Seq Error)
type InstsAndErrors = ([(Identifier, Type)], Seq Error)
type UniquesAndErrors = (Map Identifier Identifier, Seq Error)
type AttrOverwrite  = Map AttrName Bool
type AccumRuleCheck = (RulesAndErrors, AttrOverwrite)
type AccumDefiCheck = (Seq Error, AttrOverwrite, [AttrName], [AttrName])

checkRules :: Map NontermIdent (Attributes, Attributes) -> Map NontermIdent (Map ConstructorIdent FieldMap) ->
              Map NontermIdent (Map ConstructorIdent [Identifier]) -> Map NontermIdent (Map ConstructorIdent [SigInfo]) ->
              NontermIdent -> ConstructorIdent -> [RuleInfo] -> RulesAndErrors
checkRules attributes fields allinsts allsigs nt con rs
  = let fieldmap :: FieldMap
        fieldmap = (_LHS,NT nt undefined) : (_LOC,NT undefined undefined) : (_INST, NT undefined undefined) : (_FIRST, NT undefined undefined) : (_LAST, NT undefined undefined)
                 : Map.findWithDefault [] con (Map.findWithDefault Map.empty nt fields)
                 ++ mapMaybe (\instNm -> lookup instNm sigs >>= \tp -> return (instNm, tp)) (Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allinsts))
        
        sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allsigs)

        hasAttrib f tp attr  = Map.member attr (f (Map.findWithDefault (Map.empty,Map.empty) tp attributes))
  
        checkRule :: RuleInfo -> AccumRuleCheck -> AccumRuleCheck
        checkRule (pat,exp,as,owrt,str) ((r1,e1),m1) 
          = let (e2,m2,u2,b2) = foldr (checkDefi owrt) (e1,m1,[],[]) as
            in  ( (Rule (pat u2) exp owrt str : r1, e2), m2)

        checkDefi :: Bool -> AttrName -> AccumDefiCheck -> AccumDefiCheck
        checkDefi owrt fa@(field,attr) (e,m,u,bs)
         = case lookup field fieldmap
            of  Just (NT tp _) -> if field == _LOC || field == _INST || field == _FIRST || field == _LAST
                                     || hasAttrib (if getName field==getName _LHS then snd else fst) tp attr
                                  then case Map.lookupIndex fa m of
                                           Just ix -> let ((_,attr2),b) = Map.elemAt ix m
                                                       in  if b && not (fa `elem` bs)
                                                           then (                                             e, Map.insert fa owrt m, fa:u, fa:bs)
                                                           else (((Seq.<|)) (DupRule nt con field attr2 attr)   e,                    m, fa:u,    bs)
                                           Nothing ->           (                                             e, Map.insert fa owrt m,    u, fa:bs)
                                  else                          (((Seq.<|)) (SuperfluousRule nt con field attr) e,                    m, fa:u,    bs)
                _              ->                               (((Seq.<|)) (UndefChild nt con field)           e,                    m, fa:u,    bs )

    in  fst (foldr checkRule (([],Seq.empty),Map.empty) rs)

checkSigs :: NontermIdent -> ConstructorIdent -> [SigInfo] -> SigsAndErrors
checkSigs nt con sis 
  = let checkSig (ide,typ) (sigs,errs)
         = if   ide `elem` map (\(TypeSig n t)-> n) sigs
           then (sigs, ((Seq.<|)) (DupSig nt con ide) errs)
           -- else if not (ide `elem` locattrdefs)
           -- then (sigs, ((Seq.<|)) (SupSig nt con ide) errs)
           else (TypeSig ide typ:sigs, errs)
    in  foldr checkSig ([],Seq.empty) sis

checkInsts :: Set NontermIdent -> Map NontermIdent (Map ConstructorIdent [SigInfo]) -> NontermIdent -> ConstructorIdent -> [Identifier] -> InstsAndErrors
checkInsts allNts sigMap nt con
  = foldr (\inst (insts, errs) ->
              maybe (insts, Seq.singleton (MissingInstSig nt con inst) >< errs)
                    (\info@(_, NT nm _) -> case nm `Set.member` allNts of
                                             True  -> (info : insts, errs)
                                             False | take 2 (getName nm) == "T_" -> let nm' = Ident (drop 2 (getName nm)) (getPos nm)
                                                                                    in case nm' `Set.member` allNts of
                                                                                         True  -> (info : insts, errs)
                                                                                         False -> (insts, Seq.singleton (UndefNont nm') >< errs)
                                                   | otherwise                   -> (insts, Seq.singleton (UndefNont nm) >< errs)
                    ) 
                  $ findSig inst
          ) ([], Seq.empty)
  where
    sigs = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt sigMap)
    
    findSig name
      = do tp@(NT _ _) <- lookup name sigs
           return (name, tp)

checkUniques :: Map NontermIdent (Attributes, Attributes) -> NontermIdent -> ConstructorIdent -> [UniqueInfo] -> UniquesAndErrors
checkUniques allAttrs nt con uniques
  = let checkUnique (ident,ref) (us,errs)
          = if ident `Map.member` us
            then (us, ((Seq.<|)) (DupUnique nt con ident) errs)
            else if Map.member ref inhs && Map.member ref syns
                 then (Map.insert ident ref us, errs)
                 else (us, ((Seq.<|)) (MissingUnique nt ref) errs)

        (inhs,syns) = Map.findWithDefault (Map.empty,Map.empty) nt allAttrs
    in foldr checkUnique (Map.empty, Seq.empty) uniques


mkUniqueRules :: Options -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)]) -> Map NontermIdent (Attributes,Attributes) -> NontermIdent -> ConstructorIdent -> Map Identifier Identifier -> [Rule]
mkUniqueRules opts allFields allAttrDecls nt con usMap
  = map apply groups
  where
    fields = Map.findWithDefault [] con (Map.findWithDefault Map.empty nt allFields)

    groups = Map.assocs $ Map.foldWithKey (\i r m -> Map.insertWith (++) r [i] m) Map.empty usMap
    apply (ref,us) = mkRule ref (findOutField ref) us
    findOutField ref = case [ chld | (chld,NT tp _) <- fields, tp `hasSyn` ref] of
                         []    -> _LHS
                         (x:_) -> x
    hasSyn tp ref = Map.member ref $ snd $ Map.findWithDefault (Map.empty,Map.empty) tp allAttrDecls
    mkRule ref outFld locAttrs
      = let pat = Product noPos (attr outFld ref : [attr _LOC u | u <- locAttrs ])
            rhs = Expression noPos $ wrap ref $ foldr gencase (finalout locAttrs) locAttrs
                     -- [HsToken ("mkUniques" ++ show (length locAttrs) ++ " ") noPos, AGField _LHS ref noPos Nothing]
        in Rule pat rhs False "-- generated by the unique rule mechanism."
    attr fld a = Alias fld a (Underscore noPos) []
    gencase nm outp
      = h ("case " ++ uniqueDispenser opts ++ " __cont of { (__cont, " ++ getName nm ++ ") -> ") ++ outp ++ h "}"
    h s = [HsToken s noPos]
    finalout us = h ("(__cont, " ++ concat (intersperse "," (map getName us)) ++ ")")
    wrap ref inp = h "case " ++ [AGField _LHS ref noPos Nothing] ++ h " of { __cont | __cont `seq` True -> " ++ inp ++ h "}"


flattenDatas :: DataTypes -> Map NontermIdent (Set NontermIdent)
flattenDatas ds = Map.map flatten ds
  where flatten cs =  Set.fromList [ nt | (_,NT nt _) <- concatMap snd (Map.toList cs)]

reachableFrom :: Map NontermIdent (Set NontermIdent) -> Set NontermIdent -> Set NontermIdent
reachableFrom table nts = reach nts
  where reach nts = let nts' = Set.unions (nts : [ ns  | nt <- Set.toList nts
                                                 , let ns = Map.findWithDefault Set.empty nt table ])
                    in if Set.size nts' > Set.size nts
                          then reach nts'
                          else nts
invert :: Map NontermIdent (Set NontermIdent) -> Map NontermIdent (Set NontermIdent)
invert m = foldr inv Map.empty (Map.toList m)
  where inv (x,ns) m = fold (\n m -> Map.insertWith Set.union n (Set.singleton x) m) m ns

path :: Map NontermIdent (Set NontermIdent) -> NontermIdent -> NontermIdent -> Set NontermIdent
path table from to = let children = Map.findWithDefault Set.empty from table
                         forward  = reachableFrom table children
                         backward = reachableFrom (invert table)
                                                  (Set.singleton to)
                     in  Set.intersection forward backward


pragmaMapUnion :: PragmaMap -> PragmaMap -> PragmaMap
pragmaMapUnion = Map.unionWith (Map.unionWith Set.union)

pragmaMapSingle :: NontermIdent -> ConstructorIdent -> Set Identifier -> PragmaMap
pragmaMapSingle nt con nms = Map.singleton nt (Map.singleton con nms)


orderMapUnion :: AttrOrderMap -> AttrOrderMap -> AttrOrderMap
orderMapUnion = Map.unionWith (Map.unionWith Set.union)

orderMapSingle :: NontermIdent -> ConstructorIdent -> Set Dependency -> AttrOrderMap
orderMapSingle nt con deps = Map.singleton nt (Map.singleton con deps)


mergeParams :: ParamMap -> ParamMap -> ParamMap
mergeParams = Map.unionWith (++)


mergeCtx :: ContextMap -> ContextMap -> ContextMap
mergeCtx
  = Map.unionWith nubconcat
  where nubconcat a b = nub (a ++ b)


mergeDerivings m1 m2 = foldr (\(n,cs) m -> Map.insertWith Set.union n cs m) m2 (Map.toList m1)


merge x y = foldr f y (Map.toList x)
 where f ~(k,v) m = Map.insertWith (Map.union) k v m


checkAttrs allFields nts inherited synthesized decls = foldErrors check decls nts where
  check nt decls | not (nt `Map.member` allFields) = (decls,Seq.singleton(UndefNont nt))
                 | otherwise = let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt decls
                                   (inh',einh) = checkDuplicates (DupInhAttr nt) inherited   inh
                                   (syn',esyn) = checkDuplicates (DupSynAttr nt) synthesized syn
                               in (Map.insert nt (inh',syn') decls,einh >< esyn)


addSelf name atMap = let (eInh,eSyn) = Map.findWithDefault(Map.empty,Map.empty) name atMap
                     in  Map.insert name (eInh, Map.insert (Ident "self" noPos) (NT _SELF []) eSyn)atMap


makeType :: Set NontermIdent -> Type -> Type
makeType nts tp@(NT x _) | x == _SELF       = tp
                         | Set.member x nts = tp
                         | otherwise        = Haskell (typeToHaskellString Nothing [] tp)
makeType _   tp                             = tp


constructGrammar ::    Set NontermIdent
                    -> ParamMap
                    -> DataTypes
                    -> Map NontermIdent (Attributes, Attributes)
                    -> Map NontermIdent (Map Identifier (String, String, String))
                    -> Derivings
                    -> Set NontermIdent
                    -> Map NontermIdent (Map ConstructorIdent [Rule])
                    -> Map NontermIdent (Map ConstructorIdent [TypeSig])
                    -> Map NontermIdent (Map ConstructorIdent [(Identifier, Type)])
                    -> TypeSyns
                    -> PragmaMap
                    -> AttrOrderMap
                    -> ContextMap
                    -> UniqueMap
                    -> Grammar

constructGrammar nts ntParams gram attrs uses derivings wrappers allrules tsigs allinsts tsyns pragmaMap orderMap contextMap uniqueMap =
   let gr = [ (nt,Map.toList alts) | (nt,alts) <- Map.toList gram]
       nonts = map nont gr
       nont (nt,alts) =  let (inh,syn) = Map.findWithDefault (Map.empty,Map.empty) nt attrs
                             rmap      = Map.findWithDefault Map.empty             nt allrules
                             tsmap     = Map.findWithDefault Map.empty             nt tsigs
                             instsmap  = Map.findWithDefault Map.empty             nt allinsts
                             params    = Map.findWithDefault []                    nt ntParams
                             alt (con,flds) =
                                   let rules = maybe [] id (Map.lookup con rmap)
                                       tsigs = maybe [] id (Map.lookup con tsmap)
                                       insts = maybe [] id (Map.lookup con instsmap)
                                       cldrn = map (child False) flds ++ map (child True) insts
                                       child isVirtual (nm, tp) =
                                          let (inh,syn) = case tp of
                                                 NT nt _ -> Map.findWithDefault (Map.empty,Map.empty) nt attrs
                                                 _       -> (Map.empty,Map.empty)
                                          in Child nm tp inh syn isVirtual
                                   in Production con cldrn rules tsigs
                            in Nonterminal nt params inh syn (map alt alts)
   in Grammar tsyns uses derivings wrappers nonts pragmaMap orderMap ntParams contextMap uniqueMap


mapUnionWithSetUnion = Map.unionWith Set.union
mapUnionWithPlusPlus = Map.unionWith (++)
-- AG ----------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         blocks               : Blocks
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         output               : Grammar
         pragmas              : Options -> Options
   alternatives:
      alternative AG:
         child elems          : Elems 
         visit 0:
            local allFields   : _
            local allConstrs  : _
            local allRules    : _
            local allSigs     : _
            local allInsts    : _
            local allUniques  : _
            local allRulesErrs : _
            local allSigsErrs : _
            local allInstsErrs : _
            local allUniquesErrs : _
            local checkedRulesPre : _
            local checkedSigs : _
            local checkedInsts : _
            local checkedUniques : _
            local checkedRules : _
            local errs1       : _
            local errs2       : _
            local errs3       : _
            local errs4       : _
            local errs5       : _
            local errs6       : _
            local errs7       : _
            local allNonterminals : _
            local allAttrDecls : _
-}
-- cata
sem_AG :: AG  ->
          T_AG 
sem_AG (AG _elems )  =
    (sem_AG_AG (sem_Elems _elems ) )
-- semantic domain
newtype T_AG  = T_AG (Options ->
                      ( Blocks,(Seq Error),(Maybe (String,String,String)),Grammar,(Options -> Options)))
data Inh_AG  = Inh_AG {options_Inh_AG :: !(Options)}
data Syn_AG  = Syn_AG {blocks_Syn_AG :: !(Blocks),errors_Syn_AG :: !(Seq Error),moduleDecl_Syn_AG :: !(Maybe (String,String,String)),output_Syn_AG :: !(Grammar),pragmas_Syn_AG :: !(Options -> Options)}
wrap_AG :: T_AG  ->
           Inh_AG  ->
           Syn_AG 
wrap_AG (T_AG sem ) (Inh_AG _lhsIoptions )  =
    (let ( _lhsOblocks,_lhsOerrors,_lhsOmoduleDecl,_lhsOoutput,_lhsOpragmas) =
             (sem _lhsIoptions )
     in  (Syn_AG _lhsOblocks _lhsOerrors _lhsOmoduleDecl _lhsOoutput _lhsOpragmas ))
sem_AG_AG :: T_Elems  ->
             T_AG 
sem_AG_AG (T_Elems elems_ )  =
    (T_AG (\ _lhsIoptions ->
               (let _lhsOoutput :: Grammar
                    _lhsOerrors :: (Seq Error)
                    _elemsOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                    _elemsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                    _elemsOdefinedSets :: DefinedSets
                    _elemsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _lhsOblocks :: Blocks
                    _lhsOmoduleDecl :: (Maybe (String,String,String))
                    _lhsOpragmas :: (Options -> Options)
                    _elemsOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _elemsOallFields :: DataTypes
                    _elemsOallNonterminals :: (Set NontermIdent)
                    _elemsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                    _elemsIattrOrderCollect :: AttrOrderMap
                    _elemsIblocks :: Blocks
                    _elemsIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                    _elemsIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                    _elemsIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                    _elemsIcollectedNames :: (Set Identifier)
                    _elemsIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                    _elemsIcollectedSetNames :: (Set Identifier)
                    _elemsIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                    _elemsIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                    _elemsIctxCollect :: ContextMap
                    _elemsIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                    _elemsIderivings :: Derivings
                    _elemsIerrors :: (Seq Error)
                    _elemsImoduleDecl :: (Maybe (String,String,String))
                    _elemsIparamsCollect :: ParamMap
                    _elemsIpragmas :: (Options -> Options)
                    _elemsIsemPragmasCollect :: PragmaMap
                    _elemsItypeSyns :: TypeSyns
                    _elemsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                    _elemsIwrappers :: (Set NontermIdent)
                    -- "Transform.ag"(line 51, column 8)
                    _lhsOoutput =
                        constructGrammar _allNonterminals
                                         _elemsIparamsCollect
                                         _allFields
                                         _allAttrDecls
                                         _elemsIuseMap
                                         _elemsIderivings
                                         (if wrappers _lhsIoptions then _allNonterminals     else _elemsIwrappers)
                                         _checkedRules
                                         _checkedSigs
                                         _checkedInsts
                                         _elemsItypeSyns
                                         _elemsIsemPragmasCollect
                                         _elemsIattrOrderCollect
                                         _elemsIctxCollect
                                         _checkedUniques
                    -- "Transform.ag"(line 237, column 10)
                    _allFields =
                        let f (nt,con,fm) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con fm)
                        in  foldr f (Map.empty) _elemsIcollectedFields
                    -- "Transform.ag"(line 240, column 10)
                    _allConstrs =
                        let f (nt,con,_) = Map.insertWith (++) nt [con]
                        in  foldr f (Map.empty) _elemsIcollectedFields
                    -- "Transform.ag"(line 243, column 10)
                    _allRules =
                        let f (nt,con,r) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [r])
                        in  foldr f (Map.empty) _elemsIcollectedRules
                    -- "Transform.ag"(line 246, column 10)
                    _allSigs =
                        let f (nt,con,t) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con [t])
                            typeof nt r = Map.findWithDefault (Haskell "<unknown>") r $ fst $ Map.findWithDefault (Map.empty,Map.empty) nt _allAttrDecls
                        in  foldr f (Map.empty) ( _elemsIcollectedSigs
                                                ++ [ (nt, con, (ident,typeof nt ref))  | (nt, con, us) <- _elemsIcollectedUniques, (ident,ref) <- us ]
                                                )
                    -- "Transform.ag"(line 252, column 10)
                    _allInsts =
                        let f (nt,con,is) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con is)
                        in  foldr f (Map.empty) _elemsIcollectedInsts
                    -- "Transform.ag"(line 255, column 10)
                    _allUniques =
                        let f (nt,con,us) = Map.insertWith (Map.unionWith (++)) nt (Map.singleton con us)
                        in foldr f (Map.empty) _elemsIcollectedUniques
                    -- "Transform.ag"(line 258, column 10)
                    _allRulesErrs =
                        Map.mapWithKey (Map.mapWithKey . (checkRules _allAttrDecls _allFields _allInsts _allSigs    )) _allRules
                    -- "Transform.ag"(line 259, column 10)
                    _allSigsErrs =
                        Map.mapWithKey (Map.mapWithKey . (checkSigs                                                 )) _allSigs
                    -- "Transform.ag"(line 260, column 10)
                    _allInstsErrs =
                        Map.mapWithKey (Map.mapWithKey . (checkInsts _allNonterminals     _allSigs                  )) _allInsts
                    -- "Transform.ag"(line 261, column 10)
                    _allUniquesErrs =
                        Map.mapWithKey (Map.mapWithKey . (checkUniques _allAttrDecls                                )) _allUniques
                    -- "Transform.ag"(line 263, column 10)
                    _checkedRulesPre =
                        Map.map (Map.map fst) _allRulesErrs
                    -- "Transform.ag"(line 264, column 10)
                    _checkedSigs =
                        Map.map (Map.map fst) _allSigsErrs
                    -- "Transform.ag"(line 265, column 10)
                    _checkedInsts =
                        Map.map (Map.map fst) _allInstsErrs
                    -- "Transform.ag"(line 266, column 10)
                    _checkedUniques =
                        Map.map (Map.map fst) _allUniquesErrs
                    -- "Transform.ag"(line 267, column 10)
                    _checkedRules =
                        Map.unionWith (Map.unionWith (++)) _checkedRulesPre     (Map.mapWithKey (Map.mapWithKey . (mkUniqueRules _lhsIoptions _allFields     _allAttrDecls    )) _checkedUniques    )
                    -- "Transform.ag"(line 269, column 10)
                    _errs1 =
                        let f = checkForDuplicates (DupSynonym)
                        in  Seq.fromList . f . map fst $ _elemsItypeSyns
                    -- "Transform.ag"(line 272, column 10)
                    _errs2 =
                        let g nt (con,fm) = checkForDuplicates (DupChild nt con) (map fst fm)
                            f (nt,cfm)    = concat . map (g nt) . Map.toList $ cfm
                        in  Seq.fromList . concat . map f . Map.toList $ _allFields
                    -- "Transform.ag"(line 276, column 10)
                    _errs3 =
                        let f (nt,cons) = checkForDuplicates (DupAlt nt) cons
                        in   Seq.empty
                    -- "Transform.ag"(line 280, column 10)
                    _errs4 =
                        let  f m s = Map.fold ((><) . snd) s m
                        in Map.fold f Seq.empty _allRulesErrs
                    -- "Transform.ag"(line 283, column 10)
                    _errs5 =
                        let  f m s = Map.fold ((><) . snd) s m
                        in Map.fold f Seq.empty _allSigsErrs
                    -- "Transform.ag"(line 286, column 10)
                    _errs6 =
                        let  f m s = Map.fold ((><) . snd) s m
                        in Map.fold f Seq.empty _allInstsErrs
                    -- "Transform.ag"(line 289, column 10)
                    _errs7 =
                        let  f m s = Map.fold ((><) . snd) s m
                        in Map.fold f Seq.empty _allUniquesErrs
                    -- "Transform.ag"(line 292, column 10)
                    _lhsOerrors =
                        _elemsIerrors >< _errs1 >< _errs2 >< _errs3 >< _errs4 >< _errs5 >< _errs6 >< _errs7
                    -- "Transform.ag"(line 425, column 10)
                    _allNonterminals =
                        _elemsIcollectedNames `Set.difference` _elemsIcollectedSetNames
                    -- "Transform.ag"(line 445, column 8)
                    _elemsOallConstructors =
                        _elemsIcollectedConstructorsMap
                    -- "Transform.ag"(line 518, column 8)
                    _elemsOdefSets =
                        Map.fromList (map (\x->(x,(Set.singleton x, Set.empty))) (Set.toList _allNonterminals    ))
                    -- "Transform.ag"(line 519, column 8)
                    _elemsOdefinedSets =
                        Map.map fst _elemsIdefSets
                    -- "Transform.ag"(line 771, column 8)
                    _elemsOattrDecls =
                        Map.empty
                    -- "Transform.ag"(line 810, column 9)
                    _allAttrDecls =
                        if withSelf _lhsIoptions
                         then foldr addSelf _elemsIattrDecls (Set.toList _allNonterminals    )
                         else               _elemsIattrDecls
                    -- use rule "Transform.ag"(line 44, column 19)
                    _lhsOblocks =
                        _elemsIblocks
                    -- use rule "Transform.ag"(line 904, column 37)
                    _lhsOmoduleDecl =
                        _elemsImoduleDecl
                    -- use rule "Transform.ag"(line 600, column 34)
                    _lhsOpragmas =
                        _elemsIpragmas
                    -- copy rule (from local)
                    _elemsOallAttrDecls =
                        _allAttrDecls
                    -- copy rule (from local)
                    _elemsOallFields =
                        _allFields
                    -- copy rule (from local)
                    _elemsOallNonterminals =
                        _allNonterminals
                    ( _elemsIattrDecls,_elemsIattrOrderCollect,_elemsIblocks,_elemsIcollectedConstructorsMap,_elemsIcollectedFields,_elemsIcollectedInsts,_elemsIcollectedNames,_elemsIcollectedRules,_elemsIcollectedSetNames,_elemsIcollectedSigs,_elemsIcollectedUniques,_elemsIctxCollect,_elemsIdefSets,_elemsIderivings,_elemsIerrors,_elemsImoduleDecl,_elemsIparamsCollect,_elemsIpragmas,_elemsIsemPragmasCollect,_elemsItypeSyns,_elemsIuseMap,_elemsIwrappers) =
                        (elems_ _elemsOallAttrDecls _elemsOallConstructors _elemsOallFields _elemsOallNonterminals _elemsOattrDecls _elemsOdefSets _elemsOdefinedSets )
                in  ( _lhsOblocks,_lhsOerrors,_lhsOmoduleDecl,_lhsOoutput,_lhsOpragmas))) )
-- Alt ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
      synthesized attributes:
         collectedConstructorNames : Set ConstructorIdent
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
   alternatives:
      alternative Alt:
         child pos            : {Pos}
         child names          : ConstructorSet 
         child fields         : {Fields}
-}
-- cata
sem_Alt :: Alt  ->
           T_Alt 
sem_Alt (Alt _pos _names _fields )  =
    (sem_Alt_Alt _pos (sem_ConstructorSet _names ) _fields )
-- semantic domain
newtype T_Alt  = T_Alt ((Map NontermIdent (Set ConstructorIdent)) ->
                        (Set NontermIdent) ->
                        (Set NontermIdent) ->
                        ( (Set ConstructorIdent),([(NontermIdent, ConstructorIdent, FieldMap)])))
data Inh_Alt  = Inh_Alt {allConstructors_Inh_Alt :: !(Map NontermIdent (Set ConstructorIdent)),allNonterminals_Inh_Alt :: !(Set NontermIdent),nts_Inh_Alt :: !(Set NontermIdent)}
data Syn_Alt  = Syn_Alt {collectedConstructorNames_Syn_Alt :: !(Set ConstructorIdent),collectedFields_Syn_Alt :: !([(NontermIdent, ConstructorIdent, FieldMap)])}
wrap_Alt :: T_Alt  ->
            Inh_Alt  ->
            Syn_Alt 
wrap_Alt (T_Alt sem ) (Inh_Alt _lhsIallConstructors _lhsIallNonterminals _lhsInts )  =
    (let ( _lhsOcollectedConstructorNames,_lhsOcollectedFields) =
             (sem _lhsIallConstructors _lhsIallNonterminals _lhsInts )
     in  (Syn_Alt _lhsOcollectedConstructorNames _lhsOcollectedFields ))
sem_Alt_Alt :: Pos ->
               T_ConstructorSet  ->
               Fields ->
               T_Alt 
sem_Alt_Alt pos_ (T_ConstructorSet names_ ) fields_  =
    (T_Alt (\ _lhsIallConstructors
              _lhsIallNonterminals
              _lhsInts ->
                (let _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                     _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                     _namesIcollectedConstructorNames :: (Set ConstructorIdent)
                     _namesIconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                     _namesIerrors :: (Seq Error)
                     -- "Transform.ag"(line 222, column 10)
                     _lhsOcollectedFields =
                         let fieldTable =
                              [ (attr, makeType _lhsIallNonterminals tp)
                              | (attr, tp) <- fields_
                              ]
                         in   [ (nt, con, fieldTable)
                              | nt  <- Set.toList _lhsInts
                              , con <- Set.toList (_namesIconstructors (Map.findWithDefault Set.empty nt _lhsIallConstructors))
                              ]
                     -- use rule "Transform.ag"(line 89, column 62)
                     _lhsOcollectedConstructorNames =
                         _namesIcollectedConstructorNames
                     ( _namesIcollectedConstructorNames,_namesIconstructors,_namesIerrors) =
                         (names_ )
                 in  ( _lhsOcollectedConstructorNames,_lhsOcollectedFields))) )
-- Alts --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
      synthesized attributes:
         collectedConstructorNames : Set ConstructorIdent
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
   alternatives:
      alternative Cons:
         child hd             : Alt 
         child tl             : Alts 
      alternative Nil:
-}
-- cata
sem_Alts :: Alts  ->
            T_Alts 
sem_Alts list  =
    (Prelude.foldr sem_Alts_Cons sem_Alts_Nil (Prelude.map sem_Alt list) )
-- semantic domain
newtype T_Alts  = T_Alts ((Map NontermIdent (Set ConstructorIdent)) ->
                          (Set NontermIdent) ->
                          (Set NontermIdent) ->
                          ( (Set ConstructorIdent),([(NontermIdent, ConstructorIdent, FieldMap)])))
data Inh_Alts  = Inh_Alts {allConstructors_Inh_Alts :: !(Map NontermIdent (Set ConstructorIdent)),allNonterminals_Inh_Alts :: !(Set NontermIdent),nts_Inh_Alts :: !(Set NontermIdent)}
data Syn_Alts  = Syn_Alts {collectedConstructorNames_Syn_Alts :: !(Set ConstructorIdent),collectedFields_Syn_Alts :: !([(NontermIdent, ConstructorIdent, FieldMap)])}
wrap_Alts :: T_Alts  ->
             Inh_Alts  ->
             Syn_Alts 
wrap_Alts (T_Alts sem ) (Inh_Alts _lhsIallConstructors _lhsIallNonterminals _lhsInts )  =
    (let ( _lhsOcollectedConstructorNames,_lhsOcollectedFields) =
             (sem _lhsIallConstructors _lhsIallNonterminals _lhsInts )
     in  (Syn_Alts _lhsOcollectedConstructorNames _lhsOcollectedFields ))
sem_Alts_Cons :: T_Alt  ->
                 T_Alts  ->
                 T_Alts 
sem_Alts_Cons (T_Alt hd_ ) (T_Alts tl_ )  =
    (T_Alts (\ _lhsIallConstructors
               _lhsIallNonterminals
               _lhsInts ->
                 (let _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _hdOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _hdOallNonterminals :: (Set NontermIdent)
                      _hdOnts :: (Set NontermIdent)
                      _tlOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _tlOallNonterminals :: (Set NontermIdent)
                      _tlOnts :: (Set NontermIdent)
                      _hdIcollectedConstructorNames :: (Set ConstructorIdent)
                      _hdIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _tlIcollectedConstructorNames :: (Set ConstructorIdent)
                      _tlIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      -- use rule "Transform.ag"(line 89, column 62)
                      _lhsOcollectedConstructorNames =
                          _hdIcollectedConstructorNames `Set.union` _tlIcollectedConstructorNames
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          _hdIcollectedFields ++ _tlIcollectedFields
                      -- copy rule (down)
                      _hdOallConstructors =
                          _lhsIallConstructors
                      -- copy rule (down)
                      _hdOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _hdOnts =
                          _lhsInts
                      -- copy rule (down)
                      _tlOallConstructors =
                          _lhsIallConstructors
                      -- copy rule (down)
                      _tlOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _tlOnts =
                          _lhsInts
                      ( _hdIcollectedConstructorNames,_hdIcollectedFields) =
                          (hd_ _hdOallConstructors _hdOallNonterminals _hdOnts )
                      ( _tlIcollectedConstructorNames,_tlIcollectedFields) =
                          (tl_ _tlOallConstructors _tlOallNonterminals _tlOnts )
                  in  ( _lhsOcollectedConstructorNames,_lhsOcollectedFields))) )
sem_Alts_Nil :: T_Alts 
sem_Alts_Nil  =
    (T_Alts (\ _lhsIallConstructors
               _lhsIallNonterminals
               _lhsInts ->
                 (let _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      -- use rule "Transform.ag"(line 89, column 62)
                      _lhsOcollectedConstructorNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                  in  ( _lhsOcollectedConstructorNames,_lhsOcollectedFields))) )
-- Attrs -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         nts                  : Set NontermIdent
      chained attribute:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
      synthesized attributes:
         errors               : Seq Error
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
   alternatives:
      alternative Attrs:
         child pos            : {Pos}
         child inh            : {AttrNames}
         child chn            : {AttrNames}
         child syn            : {AttrNames}
         visit 0:
            local _tup1       : _
            local attrDecls   : _
            local errors      : _
            local _tup2       : _
            local inherited   : _
            local synthesized : _
            local useMap      : _
-}
-- cata
sem_Attrs :: Attrs  ->
             T_Attrs 
sem_Attrs (Attrs _pos _inh _chn _syn )  =
    (sem_Attrs_Attrs _pos _inh _chn _syn )
-- semantic domain
newtype T_Attrs  = T_Attrs (DataTypes ->
                            (Set NontermIdent) ->
                            (Map NontermIdent (Attributes, Attributes)) ->
                            (Set NontermIdent) ->
                            ( (Map NontermIdent (Attributes, Attributes)),(Seq Error),(Map NontermIdent (Map Identifier (String,String,String)))))
data Inh_Attrs  = Inh_Attrs {allFields_Inh_Attrs :: !(DataTypes),allNonterminals_Inh_Attrs :: !(Set NontermIdent),attrDecls_Inh_Attrs :: !(Map NontermIdent (Attributes, Attributes)),nts_Inh_Attrs :: !(Set NontermIdent)}
data Syn_Attrs  = Syn_Attrs {attrDecls_Syn_Attrs :: !(Map NontermIdent (Attributes, Attributes)),errors_Syn_Attrs :: !(Seq Error),useMap_Syn_Attrs :: !(Map NontermIdent (Map Identifier (String,String,String)))}
wrap_Attrs :: T_Attrs  ->
              Inh_Attrs  ->
              Syn_Attrs 
wrap_Attrs (T_Attrs sem ) (Inh_Attrs _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsInts )  =
    (let ( _lhsOattrDecls,_lhsOerrors,_lhsOuseMap) =
             (sem _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsInts )
     in  (Syn_Attrs _lhsOattrDecls _lhsOerrors _lhsOuseMap ))
sem_Attrs_Attrs :: Pos ->
                   AttrNames ->
                   AttrNames ->
                   AttrNames ->
                   T_Attrs 
sem_Attrs_Attrs pos_ inh_ chn_ syn_  =
    (T_Attrs (\ _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsInts ->
                  (let _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOerrors :: (Seq Error)
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       -- "Transform.ag"(line 779, column 15)
                       __tup1 =
                           checkAttrs _lhsIallFields (Set.toList _lhsInts) _inherited _synthesized _lhsIattrDecls
                       -- "Transform.ag"(line 779, column 15)
                       (_attrDecls,_) =
                           __tup1
                       -- "Transform.ag"(line 779, column 15)
                       (_,_errors) =
                           __tup1
                       -- "Transform.ag"(line 781, column 15)
                       __tup2 =
                           let splitAttrs xs = unzip [ ((n,makeType _lhsIallNonterminals t),(n,ud))
                                                     | (n,t,ud) <- xs
                                                     ]
                               (inh,_)     = splitAttrs inh_
                               (chn,uses1) = splitAttrs chn_
                               (syn,uses2) = splitAttrs syn_
                               isUse (n,(e1,e2,_)) = not (null e1 || null e2)
                           in (inh++chn,chn++syn, Map.fromList (Prelude.filter isUse (uses1++uses2)))
                       -- "Transform.ag"(line 781, column 15)
                       (_inherited,_,_) =
                           __tup2
                       -- "Transform.ag"(line 781, column 15)
                       (_,_synthesized,_) =
                           __tup2
                       -- "Transform.ag"(line 781, column 15)
                       (_,_,_useMap) =
                           __tup2
                       -- "Transform.ag"(line 789, column 11)
                       _lhsOuseMap =
                           Map.fromList (zip (Set.toList _lhsInts) (repeat _useMap))
                       -- use rule "Transform.ag"(line 42, column 19)
                       _lhsOerrors =
                           _errors
                       -- copy rule (from local)
                       _lhsOattrDecls =
                           _attrDecls
                   in  ( _lhsOattrDecls,_lhsOerrors,_lhsOuseMap))) )
-- ConstructorSet ----------------------------------------------
{-
   visit 0:
      synthesized attributes:
         collectedConstructorNames : Set ConstructorIdent
         constructors         : (Set ConstructorIdent->Set ConstructorIdent)
         errors               : Seq Error
   alternatives:
      alternative CAll:
      alternative CDifference:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
      alternative CName:
         child name           : {ConstructorIdent}
      alternative CUnion:
         child set1           : ConstructorSet 
         child set2           : ConstructorSet 
-}
-- cata
sem_ConstructorSet :: ConstructorSet  ->
                      T_ConstructorSet 
sem_ConstructorSet (CAll )  =
    (sem_ConstructorSet_CAll )
sem_ConstructorSet (CDifference _set1 _set2 )  =
    (sem_ConstructorSet_CDifference (sem_ConstructorSet _set1 ) (sem_ConstructorSet _set2 ) )
sem_ConstructorSet (CName _name )  =
    (sem_ConstructorSet_CName _name )
sem_ConstructorSet (CUnion _set1 _set2 )  =
    (sem_ConstructorSet_CUnion (sem_ConstructorSet _set1 ) (sem_ConstructorSet _set2 ) )
-- semantic domain
newtype T_ConstructorSet  = T_ConstructorSet (( (Set ConstructorIdent),((Set ConstructorIdent->Set ConstructorIdent)),(Seq Error)))
data Inh_ConstructorSet  = Inh_ConstructorSet {}
data Syn_ConstructorSet  = Syn_ConstructorSet {collectedConstructorNames_Syn_ConstructorSet :: !(Set ConstructorIdent),constructors_Syn_ConstructorSet :: !((Set ConstructorIdent->Set ConstructorIdent)),errors_Syn_ConstructorSet :: !(Seq Error)}
wrap_ConstructorSet :: T_ConstructorSet  ->
                       Inh_ConstructorSet  ->
                       Syn_ConstructorSet 
wrap_ConstructorSet (T_ConstructorSet sem ) (Inh_ConstructorSet )  =
    (let ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors) =
             (sem )
     in  (Syn_ConstructorSet _lhsOcollectedConstructorNames _lhsOconstructors _lhsOerrors ))
sem_ConstructorSet_CAll :: T_ConstructorSet 
sem_ConstructorSet_CAll  =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           -- "Transform.ag"(line 585, column 17)
                           _lhsOconstructors =
                               \ds -> ds
                           -- use rule "Transform.ag"(line 89, column 62)
                           _lhsOcollectedConstructorNames =
                               Set.empty
                           -- use rule "Transform.ag"(line 42, column 19)
                           _lhsOerrors =
                               Seq.empty
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
sem_ConstructorSet_CDifference :: T_ConstructorSet  ->
                                  T_ConstructorSet  ->
                                  T_ConstructorSet 
sem_ConstructorSet_CDifference (T_ConstructorSet set1_ ) (T_ConstructorSet set2_ )  =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           _set1IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set1Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set1Ierrors :: (Seq Error)
                           _set2IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set2Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set2Ierrors :: (Seq Error)
                           -- "Transform.ag"(line 584, column 17)
                           _lhsOconstructors =
                               \ds -> _set1Iconstructors ds `Set.difference` _set2Iconstructors ds
                           -- use rule "Transform.ag"(line 89, column 62)
                           _lhsOcollectedConstructorNames =
                               _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
                           -- use rule "Transform.ag"(line 42, column 19)
                           _lhsOerrors =
                               _set1Ierrors Seq.>< _set2Ierrors
                           ( _set1IcollectedConstructorNames,_set1Iconstructors,_set1Ierrors) =
                               (set1_ )
                           ( _set2IcollectedConstructorNames,_set2Iconstructors,_set2Ierrors) =
                               (set2_ )
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
sem_ConstructorSet_CName :: ConstructorIdent ->
                            T_ConstructorSet 
sem_ConstructorSet_CName name_  =
    (T_ConstructorSet (let _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOerrors :: (Seq Error)
                           -- "Transform.ag"(line 433, column 11)
                           _lhsOcollectedConstructorNames =
                               Set.singleton name_
                           -- "Transform.ag"(line 582, column 17)
                           _lhsOconstructors =
                               \ds -> Set.singleton name_
                           -- use rule "Transform.ag"(line 42, column 19)
                           _lhsOerrors =
                               Seq.empty
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
sem_ConstructorSet_CUnion :: T_ConstructorSet  ->
                             T_ConstructorSet  ->
                             T_ConstructorSet 
sem_ConstructorSet_CUnion (T_ConstructorSet set1_ ) (T_ConstructorSet set2_ )  =
    (T_ConstructorSet (let _lhsOconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _lhsOcollectedConstructorNames :: (Set ConstructorIdent)
                           _lhsOerrors :: (Seq Error)
                           _set1IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set1Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set1Ierrors :: (Seq Error)
                           _set2IcollectedConstructorNames :: (Set ConstructorIdent)
                           _set2Iconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                           _set2Ierrors :: (Seq Error)
                           -- "Transform.ag"(line 583, column 17)
                           _lhsOconstructors =
                               \ds -> _set1Iconstructors ds `Set.union`      _set2Iconstructors ds
                           -- use rule "Transform.ag"(line 89, column 62)
                           _lhsOcollectedConstructorNames =
                               _set1IcollectedConstructorNames `Set.union` _set2IcollectedConstructorNames
                           -- use rule "Transform.ag"(line 42, column 19)
                           _lhsOerrors =
                               _set1Ierrors Seq.>< _set2Ierrors
                           ( _set1IcollectedConstructorNames,_set1Iconstructors,_set1Ierrors) =
                               (set1_ )
                           ( _set2IcollectedConstructorNames,_set2Iconstructors,_set2Ierrors) =
                               (set2_ )
                       in  ( _lhsOcollectedConstructorNames,_lhsOconstructors,_lhsOerrors)) )
-- Elem --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         defSets              : Map Identifier (Set NontermIdent,Set Identifier)
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         blocks               : Blocks
         collectedConstructorsMap : Map NontermIdent (Set ConstructorIdent)
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedNames       : Set Identifier
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSetNames    : Set Identifier
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         ctxCollect           : ContextMap
         derivings            : Derivings
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         paramsCollect        : ParamMap
         pragmas              : Options -> Options
         semPragmasCollect    : PragmaMap
         typeSyns             : TypeSyns
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
         wrappers             : Set NontermIdent
   alternatives:
      alternative Attr:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child attrs          : Attrs 
      alternative Data:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child params         : {[Identifier]}
         child attrs          : Attrs 
         child alts           : Alts 
         child ext            : {Bool}
      alternative Deriving:
         child pos            : {Pos}
         child set            : NontSet 
         child classes        : {[NontermIdent]}
      alternative Module:
         child pos            : {Pos}
         child name           : {String}
         child exports        : {String}
         child imports        : {String}
      alternative Pragma:
         child pos            : {Pos}
         child names          : {[NontermIdent]}
      alternative Sem:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child names          : NontSet 
         child attrs          : Attrs 
         child alts           : SemAlts 
      alternative Set:
         child pos            : {Pos}
         child name           : {NontermIdent}
         child set            : NontSet 
         visit 0:
            local _tup3       : _
            local defSets2    : _
            local errs        : _
      alternative Txt:
         child pos            : {Pos}
         child name           : {Identifier}
         child mbNt           : {Maybe NontermIdent}
         child lines          : {[String]}
         visit 0:
            local blockInfo   : _
            local blockValue  : _
      alternative Type:
         child pos            : {Pos}
         child ctx            : {ClassContext}
         child name           : {NontermIdent}
         child params         : {[Identifier]}
         child type           : {ComplexType}
         visit 0:
            local expanded    : _
            local argType     : _
      alternative Wrapper:
         child pos            : {Pos}
         child set            : NontSet 
-}
-- cata
sem_Elem :: Elem  ->
            T_Elem 
sem_Elem (Attr _pos _ctx _names _attrs )  =
    (sem_Elem_Attr _pos _ctx (sem_NontSet _names ) (sem_Attrs _attrs ) )
sem_Elem (Data _pos _ctx _names _params _attrs _alts _ext )  =
    (sem_Elem_Data _pos _ctx (sem_NontSet _names ) _params (sem_Attrs _attrs ) (sem_Alts _alts ) _ext )
sem_Elem (Deriving _pos _set _classes )  =
    (sem_Elem_Deriving _pos (sem_NontSet _set ) _classes )
sem_Elem (Module _pos _name _exports _imports )  =
    (sem_Elem_Module _pos _name _exports _imports )
sem_Elem (Pragma _pos _names )  =
    (sem_Elem_Pragma _pos _names )
sem_Elem (Sem _pos _ctx _names _attrs _alts )  =
    (sem_Elem_Sem _pos _ctx (sem_NontSet _names ) (sem_Attrs _attrs ) (sem_SemAlts _alts ) )
sem_Elem (Set _pos _name _set )  =
    (sem_Elem_Set _pos _name (sem_NontSet _set ) )
sem_Elem (Txt _pos _name _mbNt _lines )  =
    (sem_Elem_Txt _pos _name _mbNt _lines )
sem_Elem (Type _pos _ctx _name _params _type )  =
    (sem_Elem_Type _pos _ctx _name _params _type )
sem_Elem (Wrapper _pos _set )  =
    (sem_Elem_Wrapper _pos (sem_NontSet _set ) )
-- semantic domain
newtype T_Elem  = T_Elem ((Map NontermIdent (Attributes, Attributes)) ->
                          (Map NontermIdent (Set ConstructorIdent)) ->
                          DataTypes ->
                          (Set NontermIdent) ->
                          (Map NontermIdent (Attributes, Attributes)) ->
                          (Map Identifier (Set NontermIdent,Set Identifier)) ->
                          DefinedSets ->
                          ( (Map NontermIdent (Attributes, Attributes)),AttrOrderMap,Blocks,(Map NontermIdent (Set ConstructorIdent)),([(NontermIdent, ConstructorIdent, FieldMap)]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),(Set Identifier),([ (NontermIdent, ConstructorIdent, RuleInfo)]),(Set Identifier),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ContextMap,(Map Identifier (Set NontermIdent,Set Identifier)),Derivings,(Seq Error),(Maybe (String,String,String)),ParamMap,(Options -> Options),PragmaMap,TypeSyns,(Map NontermIdent (Map Identifier (String,String,String))),(Set NontermIdent)))
data Inh_Elem  = Inh_Elem {allAttrDecls_Inh_Elem :: !(Map NontermIdent (Attributes, Attributes)),allConstructors_Inh_Elem :: !(Map NontermIdent (Set ConstructorIdent)),allFields_Inh_Elem :: !(DataTypes),allNonterminals_Inh_Elem :: !(Set NontermIdent),attrDecls_Inh_Elem :: !(Map NontermIdent (Attributes, Attributes)),defSets_Inh_Elem :: !(Map Identifier (Set NontermIdent,Set Identifier)),definedSets_Inh_Elem :: !(DefinedSets)}
data Syn_Elem  = Syn_Elem {attrDecls_Syn_Elem :: !(Map NontermIdent (Attributes, Attributes)),attrOrderCollect_Syn_Elem :: !(AttrOrderMap),blocks_Syn_Elem :: !(Blocks),collectedConstructorsMap_Syn_Elem :: !(Map NontermIdent (Set ConstructorIdent)),collectedFields_Syn_Elem :: !([(NontermIdent, ConstructorIdent, FieldMap)]),collectedInsts_Syn_Elem :: !([ (NontermIdent, ConstructorIdent, [Identifier]) ]),collectedNames_Syn_Elem :: !(Set Identifier),collectedRules_Syn_Elem :: !([ (NontermIdent, ConstructorIdent, RuleInfo)]),collectedSetNames_Syn_Elem :: !(Set Identifier),collectedSigs_Syn_Elem :: !([ (NontermIdent, ConstructorIdent, SigInfo) ]),collectedUniques_Syn_Elem :: !([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ctxCollect_Syn_Elem :: !(ContextMap),defSets_Syn_Elem :: !(Map Identifier (Set NontermIdent,Set Identifier)),derivings_Syn_Elem :: !(Derivings),errors_Syn_Elem :: !(Seq Error),moduleDecl_Syn_Elem :: !(Maybe (String,String,String)),paramsCollect_Syn_Elem :: !(ParamMap),pragmas_Syn_Elem :: !(Options -> Options),semPragmasCollect_Syn_Elem :: !(PragmaMap),typeSyns_Syn_Elem :: !(TypeSyns),useMap_Syn_Elem :: !(Map NontermIdent (Map Identifier (String,String,String))),wrappers_Syn_Elem :: !(Set NontermIdent)}
wrap_Elem :: T_Elem  ->
             Inh_Elem  ->
             Syn_Elem 
wrap_Elem (T_Elem sem ) (Inh_Elem _lhsIallAttrDecls _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIdefSets _lhsIdefinedSets )  =
    (let ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers) =
             (sem _lhsIallAttrDecls _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIdefSets _lhsIdefinedSets )
     in  (Syn_Elem _lhsOattrDecls _lhsOattrOrderCollect _lhsOblocks _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers ))
sem_Elem_Attr :: Pos ->
                 ClassContext ->
                 T_NontSet  ->
                 T_Attrs  ->
                 T_Elem 
sem_Elem_Attr pos_ ctx_ (T_NontSet names_ ) (T_Attrs attrs_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOctxCollect :: ContextMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      -- "Transform.ag"(line 731, column 7)
                      _lhsOctxCollect =
                          if null ctx_
                          then Map.empty
                          else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                      -- "Transform.ag"(line 775, column 10)
                      _attrsOnts =
                          _namesInontSet
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          _namesIcollectedNames
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          _namesIerrors Seq.>< _attrsIerrors
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          _attrsIuseMap
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (up)
                      _lhsOattrDecls =
                          _attrsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                      -- copy rule (down)
                      _namesOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _namesOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _namesOdefinedSets =
                          _lhsIdefinedSets
                      -- copy rule (down)
                      _attrsOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _attrsOattrDecls =
                          _lhsIattrDecls
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          (names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets )
                      ( _attrsIattrDecls,_attrsIerrors,_attrsIuseMap) =
                          (attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOnts )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Data :: Pos ->
                 ClassContext ->
                 T_NontSet  ->
                 ([Identifier]) ->
                 T_Attrs  ->
                 T_Alts  ->
                 Bool ->
                 T_Elem 
sem_Elem_Data pos_ ctx_ (T_NontSet names_ ) params_ (T_Attrs attrs_ ) (T_Alts alts_ ) ext_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _altsOnts :: (Set NontermIdent)
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOctxCollect :: ContextMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _altsOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                      _altsOallNonterminals :: (Set NontermIdent)
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _altsIcollectedConstructorNames :: (Set ConstructorIdent)
                      _altsIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      -- "Transform.ag"(line 160, column 10)
                      _altsOnts =
                          _namesInontSet
                      -- "Transform.ag"(line 439, column 11)
                      _lhsOcollectedConstructorsMap =
                          Map.fromList
                          [ (n, _altsIcollectedConstructorNames)
                          | n <- Set.toList _namesInontSet
                          ]
                      -- "Transform.ag"(line 708, column 7)
                      _lhsOparamsCollect =
                          if null params_
                          then Map.empty
                          else Map.fromList [(nt, params_) | nt <- Set.toList _namesInontSet]
                      -- "Transform.ag"(line 731, column 7)
                      _lhsOctxCollect =
                          if null ctx_
                          then Map.empty
                          else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                      -- "Transform.ag"(line 774, column 10)
                      _attrsOnts =
                          _namesInontSet
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          _altsIcollectedFields
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          _namesIcollectedNames
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          _namesIerrors Seq.>< _attrsIerrors
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          _attrsIuseMap
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (up)
                      _lhsOattrDecls =
                          _attrsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                      -- copy rule (down)
                      _namesOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _namesOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _namesOdefinedSets =
                          _lhsIdefinedSets
                      -- copy rule (down)
                      _attrsOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _attrsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (down)
                      _altsOallConstructors =
                          _lhsIallConstructors
                      -- copy rule (down)
                      _altsOallNonterminals =
                          _lhsIallNonterminals
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          (names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets )
                      ( _attrsIattrDecls,_attrsIerrors,_attrsIuseMap) =
                          (attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOnts )
                      ( _altsIcollectedConstructorNames,_altsIcollectedFields) =
                          (alts_ _altsOallConstructors _altsOallNonterminals _altsOnts )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Deriving :: Pos ->
                     T_NontSet  ->
                     ([NontermIdent]) ->
                     T_Elem 
sem_Elem_Deriving pos_ (T_NontSet set_ ) classes_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOderivings :: Derivings
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "Transform.ag"(line 759, column 14)
                      _lhsOderivings =
                          Map.fromList [(nt,Set.fromList classes_) | nt <- Set.toList _setInontSet]
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          _setIcollectedNames
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 727, column 34)
                      _lhsOctxCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          _setIerrors
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                      -- copy rule (down)
                      _setOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _setOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _setOdefinedSets =
                          _lhsIdefinedSets
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          (set_ _setOallFields _setOallNonterminals _setOdefinedSets )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Module :: Pos ->
                   String ->
                   String ->
                   String ->
                   T_Elem 
sem_Elem_Module pos_ name_ exports_ imports_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "Transform.ag"(line 908, column 7)
                      _lhsOmoduleDecl =
                          Just (name_, exports_, imports_)
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 727, column 34)
                      _lhsOctxCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          Seq.empty
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Pragma :: Pos ->
                   ([NontermIdent]) ->
                   T_Elem 
sem_Elem_Pragma pos_ names_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOpragmas :: (Options -> Options)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "Transform.ag"(line 603, column 13)
                      _lhsOpragmas =
                          let mk n o = case getName n of
                                         "gencatas"     -> o { folds       = True  }
                                         "nogencatas"   -> o { folds       = False }
                                         "gendatas"     -> o { dataTypes   = True  }
                                         "nogendatas"   -> o { dataTypes   = False }
                                         "gensems"      -> o { semfuns     = True  }
                                         "nogensems"    -> o { semfuns     = False }
                                         "gentypesigs"  -> o { typeSigs    = True  }
                                         "nogentypesigs"-> o { typeSigs    = False }
                                         "nocycle"      -> o { withCycle   = False }
                                         "cycle"        -> o { withCycle   = True  }
                                         "nostrictdata" -> o { strictData  = False }
                                         "strictdata"   -> o { strictData  = True  }
                                         "nostrictcase" -> o { strictCases = False }
                                         "strictcase"   -> o { strictCases = True  }
                                         "strictercase" -> o { strictCases = True, stricterCases = True }
                                         "nostrictwrap" -> o { strictWrap  = False }
                                         "strictwrap"   -> o { strictWrap  = True  }
                                         "novisit"      -> o { visit       = False }
                                         "visit"        -> o { visit       = True  }
                                         "nocase"       -> o { cases       = False }
                                         "case"         -> o { cases       = True  }
                                         "noseq"        -> o { withSeq     = False }
                                         "seq"          -> o { withSeq     = True  }
                                         "nounbox"      -> o { unbox       = False }
                                         "unbox"        -> o { unbox       = True  }
                                         "bangpats"     -> o { bangpats    = True  }
                                         "nooptimize"   -> o { cases = False , visit = False }
                                         "optimize"     -> o { cases = True  , visit = True  }
                                         "strictsem"    -> o { strictSems = True }
                                         "gentraces"    -> o { genTraces = True }
                                         "genusetraces" -> o { genUseTraces = True }
                                         "splitsems"    -> o { splitSems = True }
                                         "gencostcentres" -> o { genCostCentres = True }
                                         "sepsemmods"   -> o { sepSemMods = True }
                                         "genlinepragmas" -> o { genLinePragmas = True }
                                         "newtypes"       -> o { newtypes = True }
                                         "nonewtypes"     -> o { newtypes = False }
                                         _              -> o
                          in \o -> foldr mk o names_
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 727, column 34)
                      _lhsOctxCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          Seq.empty
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Sem :: Pos ->
                ClassContext ->
                T_NontSet  ->
                T_Attrs  ->
                T_SemAlts  ->
                T_Elem 
sem_Elem_Sem pos_ ctx_ (T_NontSet names_ ) (T_Attrs attrs_ ) (T_SemAlts alts_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _altsOnts :: (Set NontermIdent)
                      _lhsOctxCollect :: ContextMap
                      _attrsOnts :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _namesOallFields :: DataTypes
                      _namesOallNonterminals :: (Set NontermIdent)
                      _namesOdefinedSets :: DefinedSets
                      _attrsOallFields :: DataTypes
                      _attrsOallNonterminals :: (Set NontermIdent)
                      _attrsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _altsOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _altsOallFields :: DataTypes
                      _namesIcollectedNames :: (Set Identifier)
                      _namesIerrors :: (Seq Error)
                      _namesInontSet :: (Set NontermIdent)
                      _attrsIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _attrsIerrors :: (Seq Error)
                      _attrsIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _altsIattrOrderCollect :: AttrOrderMap
                      _altsIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _altsIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _altsIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _altsIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _altsIerrors :: (Seq Error)
                      _altsIsemPragmasCollect :: PragmaMap
                      -- "Transform.ag"(line 161, column 10)
                      _altsOnts =
                          _namesInontSet
                      -- "Transform.ag"(line 731, column 7)
                      _lhsOctxCollect =
                          if null ctx_
                          then Map.empty
                          else Map.fromList [(nt, ctx_) | nt <- Set.toList _namesInontSet]
                      -- "Transform.ag"(line 776, column 10)
                      _attrsOnts =
                          _namesInontSet
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          _altsIattrOrderCollect
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          _altsIcollectedInsts
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          _namesIcollectedNames
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          _altsIcollectedRules
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          _altsIcollectedSigs
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          _altsIcollectedUniques
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          _namesIerrors Seq.>< _attrsIerrors Seq.>< _altsIerrors
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          _altsIsemPragmasCollect
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          _attrsIuseMap
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (up)
                      _lhsOattrDecls =
                          _attrsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                      -- copy rule (down)
                      _namesOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _namesOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _namesOdefinedSets =
                          _lhsIdefinedSets
                      -- copy rule (down)
                      _attrsOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _attrsOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _attrsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (down)
                      _altsOallAttrDecls =
                          _lhsIallAttrDecls
                      -- copy rule (down)
                      _altsOallFields =
                          _lhsIallFields
                      ( _namesIcollectedNames,_namesIerrors,_namesInontSet) =
                          (names_ _namesOallFields _namesOallNonterminals _namesOdefinedSets )
                      ( _attrsIattrDecls,_attrsIerrors,_attrsIuseMap) =
                          (attrs_ _attrsOallFields _attrsOallNonterminals _attrsOattrDecls _attrsOnts )
                      ( _altsIattrOrderCollect,_altsIcollectedInsts,_altsIcollectedRules,_altsIcollectedSigs,_altsIcollectedUniques,_altsIerrors,_altsIsemPragmasCollect) =
                          (alts_ _altsOallAttrDecls _altsOallFields _altsOnts )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Set :: Pos ->
                NontermIdent ->
                T_NontSet  ->
                T_Elem 
sem_Elem_Set pos_ name_ (T_NontSet set_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _lhsOerrors :: (Seq Error)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "Transform.ag"(line 416, column 10)
                      _lhsOcollectedSetNames =
                          Set.singleton name_
                      -- "Transform.ag"(line 523, column 13)
                      __tup3 =
                          let allUsedNames = Set.unions [ maybe (Set.singleton n)
                                                                snd
                                                                (Map.lookup n _lhsIdefSets)
                                                        | n <- Set.toList _setIcollectedNames
                                                        ]
                              (nontSet,e1) | Set.member name_ allUsedNames
                                                       = (Set.empty, Seq.singleton(CyclicSet name_))
                                           | otherwise = (_setInontSet, Seq.empty)
                              (res, e2) = checkDuplicate DupSet  name_ (nontSet,Set.insert name_ allUsedNames) _lhsIdefSets
                          in (res, e1 Seq.>< e2)
                      -- "Transform.ag"(line 523, column 13)
                      (_defSets2,_) =
                          __tup3
                      -- "Transform.ag"(line 523, column 13)
                      (_,_errs) =
                          __tup3
                      -- "Transform.ag"(line 533, column 9)
                      _lhsOdefSets =
                          _defSets2
                      -- "Transform.ag"(line 533, column 9)
                      _lhsOerrors =
                          _errs >< _setIerrors
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          _setIcollectedNames
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 727, column 34)
                      _lhsOctxCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (down)
                      _setOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _setOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _setOdefinedSets =
                          _lhsIdefinedSets
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          (set_ _setOallFields _setOallNonterminals _setOdefinedSets )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Txt :: Pos ->
                Identifier ->
                (Maybe NontermIdent) ->
                ([String]) ->
                T_Elem 
sem_Elem_Txt pos_ name_ mbNt_ lines_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOblocks :: Blocks
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "Transform.ag"(line 170, column 10)
                      _blockInfo =
                          ( let nm = getName name_
                            in if nm == "imports"
                               then BlockImport
                               else if nm == "optpragmas"
                                    then BlockPragma
                                    else BlockOther
                          , mbNt_
                          )
                      -- "Transform.ag"(line 178, column 10)
                      _blockValue =
                          [(lines_, pos_)]
                      -- "Transform.ag"(line 179, column 10)
                      _lhsOblocks =
                          Map.singleton _blockInfo     _blockValue
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 727, column 34)
                      _lhsOctxCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          Seq.empty
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Type :: Pos ->
                 ClassContext ->
                 NontermIdent ->
                 ([Identifier]) ->
                 ComplexType ->
                 T_Elem 
sem_Elem_Type pos_ ctx_ name_ params_ type_  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOparamsCollect :: ParamMap
                      _lhsOctxCollect :: ContextMap
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      -- "Transform.ag"(line 232, column 10)
                      _lhsOcollectedFields =
                          map (\(x,y)->(name_, x, y)) _expanded
                      -- "Transform.ag"(line 419, column 11)
                      _lhsOcollectedNames =
                          Set.singleton name_
                      -- "Transform.ag"(line 473, column 11)
                      _expanded =
                          case _argType of
                                  List tp -> [(Ident "Cons" pos_, [(Ident "hd" pos_, tp)
                                                                  ,(Ident "tl" pos_, NT name_ (map getName params_))
                                                                  ]
                                              )
                                             ,(Ident "Nil" pos_,  [])
                                             ]
                                  Maybe tp -> [(Ident "Just" pos_, [(Ident "just" pos_, tp)
                                                                  ]
                                              )
                                             ,(Ident "Nothing" pos_,  [])
                                             ]
                                  Either tp1 tp2 -> [
                                               (Ident "Left"    pos_,  [(Ident "left"  pos_, tp1) ])
                                             , (Ident "Right"   pos_,  [(Ident "right" pos_, tp2) ])
                                             ]
                                  Map tp1 tp2 -> [ (Ident "Entry" pos_, [ (Ident "key" pos_, tp1)
                                                                        , (Ident "val" pos_, tp2)
                                                                        , (Ident "tl" pos_, NT name_ (map getName params_))
                                                                        ])
                                                 , (Ident "Nil" pos_, [])
                                                 ]
                                  IntMap tp   -> [ (Ident "Entry" pos_, [ (Ident "key" pos_, Haskell "Int")
                                                                        , (Ident "val" pos_, tp)
                                                                        , (Ident "tl" pos_, NT name_ (map getName params_))
                                                                        ])
                                                 , (Ident "Nil" pos_, [])
                                                 ]
                                  Tuple xs -> [(Ident "Tuple" pos_, xs)]
                      -- "Transform.ag"(line 502, column 11)
                      _argType =
                          case type_ of
                           Maybe tp       -> Maybe  (  makeType _lhsIallNonterminals tp)
                           Either tp1 tp2 -> Either (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                           List tp        -> List   (  makeType _lhsIallNonterminals tp)
                           Tuple xs       -> Tuple [(f,makeType _lhsIallNonterminals tp) | (f,tp) <- xs]
                           Map tp1 tp2    -> Map    (  makeType _lhsIallNonterminals tp1) (makeType _lhsIallNonterminals tp2)
                           IntMap tp      -> IntMap (  makeType _lhsIallNonterminals tp)
                      -- "Transform.ag"(line 509, column 11)
                      _lhsOtypeSyns =
                          [(name_,_argType)]
                      -- "Transform.ag"(line 714, column 7)
                      _lhsOparamsCollect =
                          if null params_
                          then Map.empty
                          else Map.singleton name_ params_
                      -- "Transform.ag"(line 737, column 7)
                      _lhsOctxCollect =
                          if null ctx_
                          then Map.empty
                          else Map.singleton name_ ctx_
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          Seq.empty
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 591, column 32)
                      _lhsOwrappers =
                          Set.empty
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elem_Wrapper :: Pos ->
                    T_NontSet  ->
                    T_Elem 
sem_Elem_Wrapper pos_ (T_NontSet set_ )  =
    (T_Elem (\ _lhsIallAttrDecls
               _lhsIallConstructors
               _lhsIallFields
               _lhsIallNonterminals
               _lhsIattrDecls
               _lhsIdefSets
               _lhsIdefinedSets ->
                 (let _lhsOwrappers :: (Set NontermIdent)
                      _lhsOattrOrderCollect :: AttrOrderMap
                      _lhsOblocks :: Blocks
                      _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                      _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                      _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                      _lhsOcollectedNames :: (Set Identifier)
                      _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                      _lhsOcollectedSetNames :: (Set Identifier)
                      _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                      _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                      _lhsOctxCollect :: ContextMap
                      _lhsOderivings :: Derivings
                      _lhsOerrors :: (Seq Error)
                      _lhsOmoduleDecl :: (Maybe (String,String,String))
                      _lhsOparamsCollect :: ParamMap
                      _lhsOpragmas :: (Options -> Options)
                      _lhsOsemPragmasCollect :: PragmaMap
                      _lhsOtypeSyns :: TypeSyns
                      _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                      _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                      _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                      _setOallFields :: DataTypes
                      _setOallNonterminals :: (Set NontermIdent)
                      _setOdefinedSets :: DefinedSets
                      _setIcollectedNames :: (Set Identifier)
                      _setIerrors :: (Seq Error)
                      _setInontSet :: (Set NontermIdent)
                      -- "Transform.ag"(line 594, column 13)
                      _lhsOwrappers =
                          _setInontSet
                      -- use rule "Transform.ag"(line 672, column 55)
                      _lhsOattrOrderCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 44, column 19)
                      _lhsOblocks =
                          Map.empty
                      -- use rule "Transform.ag"(line 90, column 48)
                      _lhsOcollectedConstructorsMap =
                          Map.empty
                      -- use rule "Transform.ag"(line 121, column 28)
                      _lhsOcollectedFields =
                          []
                      -- use rule "Transform.ag"(line 146, column 31)
                      _lhsOcollectedInsts =
                          []
                      -- use rule "Transform.ag"(line 82, column 50)
                      _lhsOcollectedNames =
                          _setIcollectedNames
                      -- use rule "Transform.ag"(line 144, column 31)
                      _lhsOcollectedRules =
                          []
                      -- use rule "Transform.ag"(line 81, column 50)
                      _lhsOcollectedSetNames =
                          Set.empty
                      -- use rule "Transform.ag"(line 145, column 31)
                      _lhsOcollectedSigs =
                          []
                      -- use rule "Transform.ag"(line 147, column 31)
                      _lhsOcollectedUniques =
                          []
                      -- use rule "Transform.ag"(line 727, column 34)
                      _lhsOctxCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 752, column 33)
                      _lhsOderivings =
                          Map.empty
                      -- use rule "Transform.ag"(line 42, column 19)
                      _lhsOerrors =
                          _setIerrors
                      -- use rule "Transform.ag"(line 904, column 37)
                      _lhsOmoduleDecl =
                          mzero
                      -- use rule "Transform.ag"(line 704, column 37)
                      _lhsOparamsCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 600, column 34)
                      _lhsOpragmas =
                          id
                      -- use rule "Transform.ag"(line 644, column 56)
                      _lhsOsemPragmasCollect =
                          Map.empty
                      -- use rule "Transform.ag"(line 460, column 32)
                      _lhsOtypeSyns =
                          []
                      -- use rule "Transform.ag"(line 133, column 15)
                      _lhsOuseMap =
                          Map.empty
                      -- copy rule (chain)
                      _lhsOattrDecls =
                          _lhsIattrDecls
                      -- copy rule (chain)
                      _lhsOdefSets =
                          _lhsIdefSets
                      -- copy rule (down)
                      _setOallFields =
                          _lhsIallFields
                      -- copy rule (down)
                      _setOallNonterminals =
                          _lhsIallNonterminals
                      -- copy rule (down)
                      _setOdefinedSets =
                          _lhsIdefinedSets
                      ( _setIcollectedNames,_setIerrors,_setInontSet) =
                          (set_ _setOallFields _setOallNonterminals _setOdefinedSets )
                  in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
-- Elems -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allConstructors      : Map NontermIdent (Set ConstructorIdent)
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
      chained attributes:
         attrDecls            : Map NontermIdent (Attributes, Attributes)
         defSets              : Map Identifier (Set NontermIdent,Set Identifier)
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         blocks               : Blocks
         collectedConstructorsMap : Map NontermIdent (Set ConstructorIdent)
         collectedFields      : [(NontermIdent, ConstructorIdent, FieldMap)]
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedNames       : Set Identifier
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSetNames    : Set Identifier
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         ctxCollect           : ContextMap
         derivings            : Derivings
         errors               : Seq Error
         moduleDecl           : Maybe (String,String,String)
         paramsCollect        : ParamMap
         pragmas              : Options -> Options
         semPragmasCollect    : PragmaMap
         typeSyns             : TypeSyns
         useMap               : Map NontermIdent (Map Identifier (String,String,String))
         wrappers             : Set NontermIdent
   alternatives:
      alternative Cons:
         child hd             : Elem 
         child tl             : Elems 
      alternative Nil:
-}
-- cata
sem_Elems :: Elems  ->
             T_Elems 
sem_Elems list  =
    (Prelude.foldr sem_Elems_Cons sem_Elems_Nil (Prelude.map sem_Elem list) )
-- semantic domain
newtype T_Elems  = T_Elems ((Map NontermIdent (Attributes, Attributes)) ->
                            (Map NontermIdent (Set ConstructorIdent)) ->
                            DataTypes ->
                            (Set NontermIdent) ->
                            (Map NontermIdent (Attributes, Attributes)) ->
                            (Map Identifier (Set NontermIdent,Set Identifier)) ->
                            DefinedSets ->
                            ( (Map NontermIdent (Attributes, Attributes)),AttrOrderMap,Blocks,(Map NontermIdent (Set ConstructorIdent)),([(NontermIdent, ConstructorIdent, FieldMap)]),([ (NontermIdent, ConstructorIdent, [Identifier]) ]),(Set Identifier),([ (NontermIdent, ConstructorIdent, RuleInfo)]),(Set Identifier),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ContextMap,(Map Identifier (Set NontermIdent,Set Identifier)),Derivings,(Seq Error),(Maybe (String,String,String)),ParamMap,(Options -> Options),PragmaMap,TypeSyns,(Map NontermIdent (Map Identifier (String,String,String))),(Set NontermIdent)))
data Inh_Elems  = Inh_Elems {allAttrDecls_Inh_Elems :: !(Map NontermIdent (Attributes, Attributes)),allConstructors_Inh_Elems :: !(Map NontermIdent (Set ConstructorIdent)),allFields_Inh_Elems :: !(DataTypes),allNonterminals_Inh_Elems :: !(Set NontermIdent),attrDecls_Inh_Elems :: !(Map NontermIdent (Attributes, Attributes)),defSets_Inh_Elems :: !(Map Identifier (Set NontermIdent,Set Identifier)),definedSets_Inh_Elems :: !(DefinedSets)}
data Syn_Elems  = Syn_Elems {attrDecls_Syn_Elems :: !(Map NontermIdent (Attributes, Attributes)),attrOrderCollect_Syn_Elems :: !(AttrOrderMap),blocks_Syn_Elems :: !(Blocks),collectedConstructorsMap_Syn_Elems :: !(Map NontermIdent (Set ConstructorIdent)),collectedFields_Syn_Elems :: !([(NontermIdent, ConstructorIdent, FieldMap)]),collectedInsts_Syn_Elems :: !([ (NontermIdent, ConstructorIdent, [Identifier]) ]),collectedNames_Syn_Elems :: !(Set Identifier),collectedRules_Syn_Elems :: !([ (NontermIdent, ConstructorIdent, RuleInfo)]),collectedSetNames_Syn_Elems :: !(Set Identifier),collectedSigs_Syn_Elems :: !([ (NontermIdent, ConstructorIdent, SigInfo) ]),collectedUniques_Syn_Elems :: !([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),ctxCollect_Syn_Elems :: !(ContextMap),defSets_Syn_Elems :: !(Map Identifier (Set NontermIdent,Set Identifier)),derivings_Syn_Elems :: !(Derivings),errors_Syn_Elems :: !(Seq Error),moduleDecl_Syn_Elems :: !(Maybe (String,String,String)),paramsCollect_Syn_Elems :: !(ParamMap),pragmas_Syn_Elems :: !(Options -> Options),semPragmasCollect_Syn_Elems :: !(PragmaMap),typeSyns_Syn_Elems :: !(TypeSyns),useMap_Syn_Elems :: !(Map NontermIdent (Map Identifier (String,String,String))),wrappers_Syn_Elems :: !(Set NontermIdent)}
wrap_Elems :: T_Elems  ->
              Inh_Elems  ->
              Syn_Elems 
wrap_Elems (T_Elems sem ) (Inh_Elems _lhsIallAttrDecls _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIdefSets _lhsIdefinedSets )  =
    (let ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers) =
             (sem _lhsIallAttrDecls _lhsIallConstructors _lhsIallFields _lhsIallNonterminals _lhsIattrDecls _lhsIdefSets _lhsIdefinedSets )
     in  (Syn_Elems _lhsOattrDecls _lhsOattrOrderCollect _lhsOblocks _lhsOcollectedConstructorsMap _lhsOcollectedFields _lhsOcollectedInsts _lhsOcollectedNames _lhsOcollectedRules _lhsOcollectedSetNames _lhsOcollectedSigs _lhsOcollectedUniques _lhsOctxCollect _lhsOdefSets _lhsOderivings _lhsOerrors _lhsOmoduleDecl _lhsOparamsCollect _lhsOpragmas _lhsOsemPragmasCollect _lhsOtypeSyns _lhsOuseMap _lhsOwrappers ))
sem_Elems_Cons :: T_Elem  ->
                  T_Elems  ->
                  T_Elems 
sem_Elems_Cons (T_Elem hd_ ) (T_Elems tl_ )  =
    (T_Elems (\ _lhsIallAttrDecls
                _lhsIallConstructors
                _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIdefSets
                _lhsIdefinedSets ->
                  (let _lhsOattrOrderCollect :: AttrOrderMap
                       _lhsOblocks :: Blocks
                       _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _lhsOcollectedNames :: (Set Identifier)
                       _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _lhsOcollectedSetNames :: (Set Identifier)
                       _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _lhsOctxCollect :: ContextMap
                       _lhsOderivings :: Derivings
                       _lhsOerrors :: (Seq Error)
                       _lhsOmoduleDecl :: (Maybe (String,String,String))
                       _lhsOparamsCollect :: ParamMap
                       _lhsOpragmas :: (Options -> Options)
                       _lhsOsemPragmasCollect :: PragmaMap
                       _lhsOtypeSyns :: TypeSyns
                       _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOwrappers :: (Set NontermIdent)
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                       _hdOallFields :: DataTypes
                       _hdOallNonterminals :: (Set NontermIdent)
                       _hdOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdOdefinedSets :: DefinedSets
                       _tlOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlOallConstructors :: (Map NontermIdent (Set ConstructorIdent))
                       _tlOallFields :: DataTypes
                       _tlOallNonterminals :: (Set NontermIdent)
                       _tlOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _tlOdefinedSets :: DefinedSets
                       _hdIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _hdIattrOrderCollect :: AttrOrderMap
                       _hdIblocks :: Blocks
                       _hdIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _hdIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _hdIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _hdIcollectedNames :: (Set Identifier)
                       _hdIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _hdIcollectedSetNames :: (Set Identifier)
                       _hdIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _hdIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _hdIctxCollect :: ContextMap
                       _hdIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _hdIderivings :: Derivings
                       _hdIerrors :: (Seq Error)
                       _hdImoduleDecl :: (Maybe (String,String,String))
                       _hdIparamsCollect :: ParamMap
                       _hdIpragmas :: (Options -> Options)
                       _hdIsemPragmasCollect :: PragmaMap
                       _hdItypeSyns :: TypeSyns
                       _hdIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _hdIwrappers :: (Set NontermIdent)
                       _tlIattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _tlIattrOrderCollect :: AttrOrderMap
                       _tlIblocks :: Blocks
                       _tlIcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _tlIcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _tlIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _tlIcollectedNames :: (Set Identifier)
                       _tlIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _tlIcollectedSetNames :: (Set Identifier)
                       _tlIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _tlIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _tlIctxCollect :: ContextMap
                       _tlIdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       _tlIderivings :: Derivings
                       _tlIerrors :: (Seq Error)
                       _tlImoduleDecl :: (Maybe (String,String,String))
                       _tlIparamsCollect :: ParamMap
                       _tlIpragmas :: (Options -> Options)
                       _tlIsemPragmasCollect :: PragmaMap
                       _tlItypeSyns :: TypeSyns
                       _tlIuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _tlIwrappers :: (Set NontermIdent)
                       -- use rule "Transform.ag"(line 672, column 55)
                       _lhsOattrOrderCollect =
                           _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
                       -- use rule "Transform.ag"(line 44, column 19)
                       _lhsOblocks =
                           _hdIblocks `mapUnionWithPlusPlus` _tlIblocks
                       -- use rule "Transform.ag"(line 90, column 48)
                       _lhsOcollectedConstructorsMap =
                           _hdIcollectedConstructorsMap `mapUnionWithSetUnion` _tlIcollectedConstructorsMap
                       -- use rule "Transform.ag"(line 121, column 28)
                       _lhsOcollectedFields =
                           _hdIcollectedFields ++ _tlIcollectedFields
                       -- use rule "Transform.ag"(line 146, column 31)
                       _lhsOcollectedInsts =
                           _hdIcollectedInsts ++ _tlIcollectedInsts
                       -- use rule "Transform.ag"(line 82, column 50)
                       _lhsOcollectedNames =
                           _hdIcollectedNames `Set.union` _tlIcollectedNames
                       -- use rule "Transform.ag"(line 144, column 31)
                       _lhsOcollectedRules =
                           _hdIcollectedRules ++ _tlIcollectedRules
                       -- use rule "Transform.ag"(line 81, column 50)
                       _lhsOcollectedSetNames =
                           _hdIcollectedSetNames `Set.union` _tlIcollectedSetNames
                       -- use rule "Transform.ag"(line 145, column 31)
                       _lhsOcollectedSigs =
                           _hdIcollectedSigs ++ _tlIcollectedSigs
                       -- use rule "Transform.ag"(line 147, column 31)
                       _lhsOcollectedUniques =
                           _hdIcollectedUniques ++ _tlIcollectedUniques
                       -- use rule "Transform.ag"(line 727, column 34)
                       _lhsOctxCollect =
                           _hdIctxCollect `mergeCtx` _tlIctxCollect
                       -- use rule "Transform.ag"(line 752, column 33)
                       _lhsOderivings =
                           _hdIderivings `mergeDerivings` _tlIderivings
                       -- use rule "Transform.ag"(line 42, column 19)
                       _lhsOerrors =
                           _hdIerrors Seq.>< _tlIerrors
                       -- use rule "Transform.ag"(line 904, column 37)
                       _lhsOmoduleDecl =
                           _hdImoduleDecl `mplus` _tlImoduleDecl
                       -- use rule "Transform.ag"(line 704, column 37)
                       _lhsOparamsCollect =
                           _hdIparamsCollect `mergeParams` _tlIparamsCollect
                       -- use rule "Transform.ag"(line 600, column 34)
                       _lhsOpragmas =
                           _hdIpragmas . _tlIpragmas
                       -- use rule "Transform.ag"(line 644, column 56)
                       _lhsOsemPragmasCollect =
                           _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
                       -- use rule "Transform.ag"(line 460, column 32)
                       _lhsOtypeSyns =
                           _hdItypeSyns ++ _tlItypeSyns
                       -- use rule "Transform.ag"(line 133, column 15)
                       _lhsOuseMap =
                           _hdIuseMap `merge` _tlIuseMap
                       -- use rule "Transform.ag"(line 591, column 32)
                       _lhsOwrappers =
                           _hdIwrappers `Set.union` _tlIwrappers
                       -- copy rule (up)
                       _lhsOattrDecls =
                           _tlIattrDecls
                       -- copy rule (up)
                       _lhsOdefSets =
                           _tlIdefSets
                       -- copy rule (down)
                       _hdOallAttrDecls =
                           _lhsIallAttrDecls
                       -- copy rule (down)
                       _hdOallConstructors =
                           _lhsIallConstructors
                       -- copy rule (down)
                       _hdOallFields =
                           _lhsIallFields
                       -- copy rule (down)
                       _hdOallNonterminals =
                           _lhsIallNonterminals
                       -- copy rule (down)
                       _hdOattrDecls =
                           _lhsIattrDecls
                       -- copy rule (down)
                       _hdOdefSets =
                           _lhsIdefSets
                       -- copy rule (down)
                       _hdOdefinedSets =
                           _lhsIdefinedSets
                       -- copy rule (down)
                       _tlOallAttrDecls =
                           _lhsIallAttrDecls
                       -- copy rule (down)
                       _tlOallConstructors =
                           _lhsIallConstructors
                       -- copy rule (down)
                       _tlOallFields =
                           _lhsIallFields
                       -- copy rule (down)
                       _tlOallNonterminals =
                           _lhsIallNonterminals
                       -- copy rule (chain)
                       _tlOattrDecls =
                           _hdIattrDecls
                       -- copy rule (chain)
                       _tlOdefSets =
                           _hdIdefSets
                       -- copy rule (down)
                       _tlOdefinedSets =
                           _lhsIdefinedSets
                       ( _hdIattrDecls,_hdIattrOrderCollect,_hdIblocks,_hdIcollectedConstructorsMap,_hdIcollectedFields,_hdIcollectedInsts,_hdIcollectedNames,_hdIcollectedRules,_hdIcollectedSetNames,_hdIcollectedSigs,_hdIcollectedUniques,_hdIctxCollect,_hdIdefSets,_hdIderivings,_hdIerrors,_hdImoduleDecl,_hdIparamsCollect,_hdIpragmas,_hdIsemPragmasCollect,_hdItypeSyns,_hdIuseMap,_hdIwrappers) =
                           (hd_ _hdOallAttrDecls _hdOallConstructors _hdOallFields _hdOallNonterminals _hdOattrDecls _hdOdefSets _hdOdefinedSets )
                       ( _tlIattrDecls,_tlIattrOrderCollect,_tlIblocks,_tlIcollectedConstructorsMap,_tlIcollectedFields,_tlIcollectedInsts,_tlIcollectedNames,_tlIcollectedRules,_tlIcollectedSetNames,_tlIcollectedSigs,_tlIcollectedUniques,_tlIctxCollect,_tlIdefSets,_tlIderivings,_tlIerrors,_tlImoduleDecl,_tlIparamsCollect,_tlIpragmas,_tlIsemPragmasCollect,_tlItypeSyns,_tlIuseMap,_tlIwrappers) =
                           (tl_ _tlOallAttrDecls _tlOallConstructors _tlOallFields _tlOallNonterminals _tlOattrDecls _tlOdefSets _tlOdefinedSets )
                   in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
sem_Elems_Nil :: T_Elems 
sem_Elems_Nil  =
    (T_Elems (\ _lhsIallAttrDecls
                _lhsIallConstructors
                _lhsIallFields
                _lhsIallNonterminals
                _lhsIattrDecls
                _lhsIdefSets
                _lhsIdefinedSets ->
                  (let _lhsOattrOrderCollect :: AttrOrderMap
                       _lhsOblocks :: Blocks
                       _lhsOcollectedConstructorsMap :: (Map NontermIdent (Set ConstructorIdent))
                       _lhsOcollectedFields :: ([(NontermIdent, ConstructorIdent, FieldMap)])
                       _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                       _lhsOcollectedNames :: (Set Identifier)
                       _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                       _lhsOcollectedSetNames :: (Set Identifier)
                       _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                       _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                       _lhsOctxCollect :: ContextMap
                       _lhsOderivings :: Derivings
                       _lhsOerrors :: (Seq Error)
                       _lhsOmoduleDecl :: (Maybe (String,String,String))
                       _lhsOparamsCollect :: ParamMap
                       _lhsOpragmas :: (Options -> Options)
                       _lhsOsemPragmasCollect :: PragmaMap
                       _lhsOtypeSyns :: TypeSyns
                       _lhsOuseMap :: (Map NontermIdent (Map Identifier (String,String,String)))
                       _lhsOwrappers :: (Set NontermIdent)
                       _lhsOattrDecls :: (Map NontermIdent (Attributes, Attributes))
                       _lhsOdefSets :: (Map Identifier (Set NontermIdent,Set Identifier))
                       -- use rule "Transform.ag"(line 672, column 55)
                       _lhsOattrOrderCollect =
                           Map.empty
                       -- use rule "Transform.ag"(line 44, column 19)
                       _lhsOblocks =
                           Map.empty
                       -- use rule "Transform.ag"(line 90, column 48)
                       _lhsOcollectedConstructorsMap =
                           Map.empty
                       -- use rule "Transform.ag"(line 121, column 28)
                       _lhsOcollectedFields =
                           []
                       -- use rule "Transform.ag"(line 146, column 31)
                       _lhsOcollectedInsts =
                           []
                       -- use rule "Transform.ag"(line 82, column 50)
                       _lhsOcollectedNames =
                           Set.empty
                       -- use rule "Transform.ag"(line 144, column 31)
                       _lhsOcollectedRules =
                           []
                       -- use rule "Transform.ag"(line 81, column 50)
                       _lhsOcollectedSetNames =
                           Set.empty
                       -- use rule "Transform.ag"(line 145, column 31)
                       _lhsOcollectedSigs =
                           []
                       -- use rule "Transform.ag"(line 147, column 31)
                       _lhsOcollectedUniques =
                           []
                       -- use rule "Transform.ag"(line 727, column 34)
                       _lhsOctxCollect =
                           Map.empty
                       -- use rule "Transform.ag"(line 752, column 33)
                       _lhsOderivings =
                           Map.empty
                       -- use rule "Transform.ag"(line 42, column 19)
                       _lhsOerrors =
                           Seq.empty
                       -- use rule "Transform.ag"(line 904, column 37)
                       _lhsOmoduleDecl =
                           mzero
                       -- use rule "Transform.ag"(line 704, column 37)
                       _lhsOparamsCollect =
                           Map.empty
                       -- use rule "Transform.ag"(line 600, column 34)
                       _lhsOpragmas =
                           id
                       -- use rule "Transform.ag"(line 644, column 56)
                       _lhsOsemPragmasCollect =
                           Map.empty
                       -- use rule "Transform.ag"(line 460, column 32)
                       _lhsOtypeSyns =
                           []
                       -- use rule "Transform.ag"(line 133, column 15)
                       _lhsOuseMap =
                           Map.empty
                       -- use rule "Transform.ag"(line 591, column 32)
                       _lhsOwrappers =
                           Set.empty
                       -- copy rule (chain)
                       _lhsOattrDecls =
                           _lhsIattrDecls
                       -- copy rule (chain)
                       _lhsOdefSets =
                           _lhsIdefSets
                   in  ( _lhsOattrDecls,_lhsOattrOrderCollect,_lhsOblocks,_lhsOcollectedConstructorsMap,_lhsOcollectedFields,_lhsOcollectedInsts,_lhsOcollectedNames,_lhsOcollectedRules,_lhsOcollectedSetNames,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOctxCollect,_lhsOdefSets,_lhsOderivings,_lhsOerrors,_lhsOmoduleDecl,_lhsOparamsCollect,_lhsOpragmas,_lhsOsemPragmasCollect,_lhsOtypeSyns,_lhsOuseMap,_lhsOwrappers))) )
-- NontSet -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allFields            : DataTypes
         allNonterminals      : Set NontermIdent
         definedSets          : DefinedSets
      synthesized attributes:
         collectedNames       : Set Identifier
         errors               : Seq Error
         nontSet              : Set NontermIdent
   alternatives:
      alternative All:
      alternative Difference:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative Intersect:
         child set1           : NontSet 
         child set2           : NontSet 
      alternative NamedSet:
         child name           : {NontermIdent}
         visit 0:
            local _tup4       : _
            local nontSet     : _
            local errors      : _
      alternative Path:
         child from           : {NontermIdent}
         child to             : {NontermIdent}
      alternative Union:
         child set1           : NontSet 
         child set2           : NontSet 
-}
-- cata
sem_NontSet :: NontSet  ->
               T_NontSet 
sem_NontSet (All )  =
    (sem_NontSet_All )
sem_NontSet (Difference _set1 _set2 )  =
    (sem_NontSet_Difference (sem_NontSet _set1 ) (sem_NontSet _set2 ) )
sem_NontSet (Intersect _set1 _set2 )  =
    (sem_NontSet_Intersect (sem_NontSet _set1 ) (sem_NontSet _set2 ) )
sem_NontSet (NamedSet _name )  =
    (sem_NontSet_NamedSet _name )
sem_NontSet (Path _from _to )  =
    (sem_NontSet_Path _from _to )
sem_NontSet (Union _set1 _set2 )  =
    (sem_NontSet_Union (sem_NontSet _set1 ) (sem_NontSet _set2 ) )
-- semantic domain
newtype T_NontSet  = T_NontSet (DataTypes ->
                                (Set NontermIdent) ->
                                DefinedSets ->
                                ( (Set Identifier),(Seq Error),(Set NontermIdent)))
data Inh_NontSet  = Inh_NontSet {allFields_Inh_NontSet :: !(DataTypes),allNonterminals_Inh_NontSet :: !(Set NontermIdent),definedSets_Inh_NontSet :: !(DefinedSets)}
data Syn_NontSet  = Syn_NontSet {collectedNames_Syn_NontSet :: !(Set Identifier),errors_Syn_NontSet :: !(Seq Error),nontSet_Syn_NontSet :: !(Set NontermIdent)}
wrap_NontSet :: T_NontSet  ->
                Inh_NontSet  ->
                Syn_NontSet 
wrap_NontSet (T_NontSet sem ) (Inh_NontSet _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets )  =
    (let ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet) =
             (sem _lhsIallFields _lhsIallNonterminals _lhsIdefinedSets )
     in  (Syn_NontSet _lhsOcollectedNames _lhsOerrors _lhsOnontSet ))
sem_NontSet_All :: T_NontSet 
sem_NontSet_All  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         -- "Transform.ag"(line 537, column 16)
                         _lhsOnontSet =
                             _lhsIallNonterminals
                         -- use rule "Transform.ag"(line 82, column 50)
                         _lhsOcollectedNames =
                             Set.empty
                         -- use rule "Transform.ag"(line 42, column 19)
                         _lhsOerrors =
                             Seq.empty
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Difference :: T_NontSet  ->
                          T_NontSet  ->
                          T_NontSet 
sem_NontSet_Difference (T_NontSet set1_ ) (T_NontSet set2_ )  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "Transform.ag"(line 543, column 16)
                         _lhsOnontSet =
                             Set.difference    _set1InontSet _set2InontSet
                         -- use rule "Transform.ag"(line 82, column 50)
                         _lhsOcollectedNames =
                             _set1IcollectedNames `Set.union` _set2IcollectedNames
                         -- use rule "Transform.ag"(line 42, column 19)
                         _lhsOerrors =
                             _set1Ierrors Seq.>< _set2Ierrors
                         -- copy rule (down)
                         _set1OallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _set1OallNonterminals =
                             _lhsIallNonterminals
                         -- copy rule (down)
                         _set1OdefinedSets =
                             _lhsIdefinedSets
                         -- copy rule (down)
                         _set2OallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _set2OallNonterminals =
                             _lhsIallNonterminals
                         -- copy rule (down)
                         _set2OdefinedSets =
                             _lhsIdefinedSets
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             (set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets )
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             (set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets )
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Intersect :: T_NontSet  ->
                         T_NontSet  ->
                         T_NontSet 
sem_NontSet_Intersect (T_NontSet set1_ ) (T_NontSet set2_ )  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "Transform.ag"(line 542, column 16)
                         _lhsOnontSet =
                             Set.intersection  _set1InontSet _set2InontSet
                         -- use rule "Transform.ag"(line 82, column 50)
                         _lhsOcollectedNames =
                             _set1IcollectedNames `Set.union` _set2IcollectedNames
                         -- use rule "Transform.ag"(line 42, column 19)
                         _lhsOerrors =
                             _set1Ierrors Seq.>< _set2Ierrors
                         -- copy rule (down)
                         _set1OallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _set1OallNonterminals =
                             _lhsIallNonterminals
                         -- copy rule (down)
                         _set1OdefinedSets =
                             _lhsIdefinedSets
                         -- copy rule (down)
                         _set2OallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _set2OallNonterminals =
                             _lhsIallNonterminals
                         -- copy rule (down)
                         _set2OdefinedSets =
                             _lhsIdefinedSets
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             (set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets )
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             (set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets )
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_NamedSet :: NontermIdent ->
                        T_NontSet 
sem_NontSet_NamedSet name_  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _lhsOnontSet :: (Set NontermIdent)
                         -- "Transform.ag"(line 422, column 14)
                         _lhsOcollectedNames =
                             Set.singleton name_
                         -- "Transform.ag"(line 538, column 20)
                         __tup4 =
                             case Map.lookup name_ _lhsIdefinedSets of
                                          Nothing  -> (Set.empty, Seq.singleton (UndefNont name_))
                                          Just set -> (set, Seq.empty)
                         -- "Transform.ag"(line 538, column 20)
                         (_nontSet,_) =
                             __tup4
                         -- "Transform.ag"(line 538, column 20)
                         (_,_errors) =
                             __tup4
                         -- use rule "Transform.ag"(line 42, column 19)
                         _lhsOerrors =
                             _errors
                         -- copy rule (from local)
                         _lhsOnontSet =
                             _nontSet
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Path :: NontermIdent ->
                    NontermIdent ->
                    T_NontSet 
sem_NontSet_Path from_ to_  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOerrors :: (Seq Error)
                         _lhsOcollectedNames :: (Set Identifier)
                         -- "Transform.ag"(line 544, column 16)
                         _lhsOnontSet =
                             let table = flattenDatas _lhsIallFields
                             in path table from_ to_
                         -- "Transform.ag"(line 546, column 16)
                         _lhsOerrors =
                             let check name | Set.member name _lhsIallNonterminals
                                                        = Seq.empty
                                            | otherwise = Seq.singleton (UndefNont name)
                             in check from_ >< check to_
                         -- use rule "Transform.ag"(line 82, column 50)
                         _lhsOcollectedNames =
                             Set.empty
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
sem_NontSet_Union :: T_NontSet  ->
                     T_NontSet  ->
                     T_NontSet 
sem_NontSet_Union (T_NontSet set1_ ) (T_NontSet set2_ )  =
    (T_NontSet (\ _lhsIallFields
                  _lhsIallNonterminals
                  _lhsIdefinedSets ->
                    (let _lhsOnontSet :: (Set NontermIdent)
                         _lhsOcollectedNames :: (Set Identifier)
                         _lhsOerrors :: (Seq Error)
                         _set1OallFields :: DataTypes
                         _set1OallNonterminals :: (Set NontermIdent)
                         _set1OdefinedSets :: DefinedSets
                         _set2OallFields :: DataTypes
                         _set2OallNonterminals :: (Set NontermIdent)
                         _set2OdefinedSets :: DefinedSets
                         _set1IcollectedNames :: (Set Identifier)
                         _set1Ierrors :: (Seq Error)
                         _set1InontSet :: (Set NontermIdent)
                         _set2IcollectedNames :: (Set Identifier)
                         _set2Ierrors :: (Seq Error)
                         _set2InontSet :: (Set NontermIdent)
                         -- "Transform.ag"(line 541, column 16)
                         _lhsOnontSet =
                             Set.union         _set1InontSet _set2InontSet
                         -- use rule "Transform.ag"(line 82, column 50)
                         _lhsOcollectedNames =
                             _set1IcollectedNames `Set.union` _set2IcollectedNames
                         -- use rule "Transform.ag"(line 42, column 19)
                         _lhsOerrors =
                             _set1Ierrors Seq.>< _set2Ierrors
                         -- copy rule (down)
                         _set1OallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _set1OallNonterminals =
                             _lhsIallNonterminals
                         -- copy rule (down)
                         _set1OdefinedSets =
                             _lhsIdefinedSets
                         -- copy rule (down)
                         _set2OallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _set2OallNonterminals =
                             _lhsIallNonterminals
                         -- copy rule (down)
                         _set2OdefinedSets =
                             _lhsIdefinedSets
                         ( _set1IcollectedNames,_set1Ierrors,_set1InontSet) =
                             (set1_ _set1OallFields _set1OallNonterminals _set1OdefinedSets )
                         ( _set2IcollectedNames,_set2Ierrors,_set2InontSet) =
                             (set2_ _set2OallFields _set2OallNonterminals _set2OdefinedSets )
                     in  ( _lhsOcollectedNames,_lhsOerrors,_lhsOnontSet))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         definedAttrs         : [AttrName]
         definedInsts         : [Identifier]
         patunder             : [AttrName]->Pattern
         stpos                : Pos
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern (Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern (Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern (Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern (Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (( Pattern,([AttrName]),([Identifier]),([AttrName]->Pattern),Pos))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),definedAttrs_Syn_Pattern :: !([AttrName]),definedInsts_Syn_Pattern :: !([Identifier]),patunder_Syn_Pattern :: !([AttrName]->Pattern),stpos_Syn_Pattern :: !(Pos)}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos) =
             (sem )
     in  (Syn_Pattern _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder _lhsOstpos ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOstpos :: Pos
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIdefinedAttrs :: ([AttrName])
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatunder :: ([AttrName]->Pattern)
                    _patIstpos :: Pos
                    _partsIcopy :: Patterns
                    _partsIdefinedAttrs :: ([AttrName])
                    _partsIdefinedInsts :: ([Identifier])
                    _partsIpatunder :: ([AttrName]->Patterns)
                    -- "Transform.ag"(line 880, column 11)
                    _lhsOdefinedAttrs =
                        (field_, attr_) : _patIdefinedAttrs
                    -- "Transform.ag"(line 881, column 11)
                    _lhsOpatunder =
                        \us -> if ((field_,attr_) `elem` us) then Underscore noPos else _copy
                    -- "Transform.ag"(line 882, column 11)
                    _lhsOdefinedInsts =
                        (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                    -- "Transform.ag"(line 897, column 16)
                    _lhsOstpos =
                        getPos field_
                    -- self rule
                    _copy =
                        Alias field_ attr_ _patIcopy _partsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patIcopy,_patIdefinedAttrs,_patIdefinedInsts,_patIpatunder,_patIstpos) =
                        (pat_ )
                    ( _partsIcopy,_partsIdefinedAttrs,_partsIdefinedInsts,_partsIpatunder) =
                        (parts_ )
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedAttrs :: ([AttrName])
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatunder :: ([AttrName]->Patterns)
                    -- "Transform.ag"(line 884, column 12)
                    _lhsOpatunder =
                        \us -> Constr name_ (_patsIpatunder us)
                    -- "Transform.ag"(line 895, column 16)
                    _lhsOstpos =
                        getPos name_
                    -- use rule "Transform.ag"(line 875, column 42)
                    _lhsOdefinedAttrs =
                        _patsIdefinedAttrs
                    -- use rule "Transform.ag"(line 874, column 55)
                    _lhsOdefinedInsts =
                        _patsIdefinedInsts
                    -- self rule
                    _copy =
                        Constr name_ _patsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patsIcopy,_patsIdefinedAttrs,_patsIdefinedInsts,_patsIpatunder) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    _lhsOstpos :: Pos
                    _patIcopy :: Pattern
                    _patIdefinedAttrs :: ([AttrName])
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatunder :: ([AttrName]->Pattern)
                    _patIstpos :: Pos
                    -- "Transform.ag"(line 886, column 17)
                    _lhsOpatunder =
                        \us -> Irrefutable (_patIpatunder us)
                    -- use rule "Transform.ag"(line 875, column 42)
                    _lhsOdefinedAttrs =
                        _patIdefinedAttrs
                    -- use rule "Transform.ag"(line 874, column 55)
                    _lhsOdefinedInsts =
                        _patIdefinedInsts
                    -- self rule
                    _copy =
                        Irrefutable _patIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    -- copy rule (up)
                    _lhsOstpos =
                        _patIstpos
                    ( _patIcopy,_patIdefinedAttrs,_patIdefinedInsts,_patIpatunder,_patIstpos) =
                        (pat_ )
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedAttrs :: ([AttrName])
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatunder :: ([AttrName]->Patterns)
                    -- "Transform.ag"(line 885, column 13)
                    _lhsOpatunder =
                        \us -> Product pos_ (_patsIpatunder us)
                    -- "Transform.ag"(line 896, column 16)
                    _lhsOstpos =
                        pos_
                    -- use rule "Transform.ag"(line 875, column 42)
                    _lhsOdefinedAttrs =
                        _patsIdefinedAttrs
                    -- use rule "Transform.ag"(line 874, column 55)
                    _lhsOdefinedInsts =
                        _patsIdefinedInsts
                    -- self rule
                    _copy =
                        Product pos_ _patsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patsIcopy,_patsIdefinedAttrs,_patsIdefinedInsts,_patsIpatunder) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOpatunder :: ([AttrName]->Pattern)
                    _lhsOstpos :: Pos
                    _lhsOdefinedAttrs :: ([AttrName])
                    _lhsOdefinedInsts :: ([Identifier])
                    _lhsOcopy :: Pattern
                    -- "Transform.ag"(line 883, column 16)
                    _lhsOpatunder =
                        \us -> _copy
                    -- "Transform.ag"(line 898, column 16)
                    _lhsOstpos =
                        pos_
                    -- use rule "Transform.ag"(line 875, column 42)
                    _lhsOdefinedAttrs =
                        []
                    -- use rule "Transform.ag"(line 874, column 55)
                    _lhsOdefinedInsts =
                        []
                    -- self rule
                    _copy =
                        Underscore pos_
                    -- self rule
                    _lhsOcopy =
                        _copy
                in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder,_lhsOstpos)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         definedAttrs         : [AttrName]
         definedInsts         : [Identifier]
         patunder             : [AttrName]->Patterns
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (( Patterns,([AttrName]),([Identifier]),([AttrName]->Patterns)))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),definedAttrs_Syn_Patterns :: !([AttrName]),definedInsts_Syn_Patterns :: !([Identifier]),patunder_Syn_Patterns :: !([AttrName]->Patterns)}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder) =
             (sem )
     in  (Syn_Patterns _lhsOcopy _lhsOdefinedAttrs _lhsOdefinedInsts _lhsOpatunder ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOpatunder :: ([AttrName]->Patterns)
                     _lhsOdefinedAttrs :: ([AttrName])
                     _lhsOdefinedInsts :: ([Identifier])
                     _lhsOcopy :: Patterns
                     _hdIcopy :: Pattern
                     _hdIdefinedAttrs :: ([AttrName])
                     _hdIdefinedInsts :: ([Identifier])
                     _hdIpatunder :: ([AttrName]->Pattern)
                     _hdIstpos :: Pos
                     _tlIcopy :: Patterns
                     _tlIdefinedAttrs :: ([AttrName])
                     _tlIdefinedInsts :: ([Identifier])
                     _tlIpatunder :: ([AttrName]->Patterns)
                     -- "Transform.ag"(line 890, column 10)
                     _lhsOpatunder =
                         \us -> (_hdIpatunder us) : (_tlIpatunder us)
                     -- use rule "Transform.ag"(line 875, column 42)
                     _lhsOdefinedAttrs =
                         _hdIdefinedAttrs ++ _tlIdefinedAttrs
                     -- use rule "Transform.ag"(line 874, column 55)
                     _lhsOdefinedInsts =
                         _hdIdefinedInsts ++ _tlIdefinedInsts
                     -- self rule
                     _copy =
                         (:) _hdIcopy _tlIcopy
                     -- self rule
                     _lhsOcopy =
                         _copy
                     ( _hdIcopy,_hdIdefinedAttrs,_hdIdefinedInsts,_hdIpatunder,_hdIstpos) =
                         (hd_ )
                     ( _tlIcopy,_tlIdefinedAttrs,_tlIdefinedInsts,_tlIpatunder) =
                         (tl_ )
                 in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOpatunder :: ([AttrName]->Patterns)
                     _lhsOdefinedAttrs :: ([AttrName])
                     _lhsOdefinedInsts :: ([Identifier])
                     _lhsOcopy :: Patterns
                     -- "Transform.ag"(line 889, column 9)
                     _lhsOpatunder =
                         \us ->  []
                     -- use rule "Transform.ag"(line 875, column 42)
                     _lhsOdefinedAttrs =
                         []
                     -- use rule "Transform.ag"(line 874, column 55)
                     _lhsOdefinedInsts =
                         []
                     -- self rule
                     _copy =
                         []
                     -- self rule
                     _lhsOcopy =
                         _copy
                 in  ( _lhsOcopy,_lhsOdefinedAttrs,_lhsOdefinedInsts,_lhsOpatunder)) )
-- SemAlt ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allFields            : DataTypes
         nts                  : Set NontermIdent
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         errors               : Seq Error
         semPragmasCollect    : PragmaMap
   alternatives:
      alternative SemAlt:
         child pos            : {Pos}
         child constructorSet : ConstructorSet 
         child rules          : SemDefs 
         visit 0:
            local pragmaNames : _
            local attrOrders  : _
            local coninfo     : _
-}
-- cata
sem_SemAlt :: SemAlt  ->
              T_SemAlt 
sem_SemAlt (SemAlt _pos _constructorSet _rules )  =
    (sem_SemAlt_SemAlt _pos (sem_ConstructorSet _constructorSet ) (sem_SemDefs _rules ) )
-- semantic domain
newtype T_SemAlt  = T_SemAlt ((Map NontermIdent (Attributes, Attributes)) ->
                              DataTypes ->
                              (Set NontermIdent) ->
                              ( AttrOrderMap,([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, RuleInfo)]),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),(Seq Error),PragmaMap))
data Inh_SemAlt  = Inh_SemAlt {allAttrDecls_Inh_SemAlt :: !(Map NontermIdent (Attributes, Attributes)),allFields_Inh_SemAlt :: !(DataTypes),nts_Inh_SemAlt :: !(Set NontermIdent)}
data Syn_SemAlt  = Syn_SemAlt {attrOrderCollect_Syn_SemAlt :: !(AttrOrderMap),collectedInsts_Syn_SemAlt :: !([ (NontermIdent, ConstructorIdent, [Identifier]) ]),collectedRules_Syn_SemAlt :: !([ (NontermIdent, ConstructorIdent, RuleInfo)]),collectedSigs_Syn_SemAlt :: !([ (NontermIdent, ConstructorIdent, SigInfo) ]),collectedUniques_Syn_SemAlt :: !([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),errors_Syn_SemAlt :: !(Seq Error),semPragmasCollect_Syn_SemAlt :: !(PragmaMap)}
wrap_SemAlt :: T_SemAlt  ->
               Inh_SemAlt  ->
               Syn_SemAlt 
wrap_SemAlt (T_SemAlt sem ) (Inh_SemAlt _lhsIallAttrDecls _lhsIallFields _lhsInts )  =
    (let ( _lhsOattrOrderCollect,_lhsOcollectedInsts,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect) =
             (sem _lhsIallAttrDecls _lhsIallFields _lhsInts )
     in  (Syn_SemAlt _lhsOattrOrderCollect _lhsOcollectedInsts _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect ))
sem_SemAlt_SemAlt :: Pos ->
                     T_ConstructorSet  ->
                     T_SemDefs  ->
                     T_SemAlt 
sem_SemAlt_SemAlt pos_ (T_ConstructorSet constructorSet_ ) (T_SemDefs rules_ )  =
    (T_SemAlt (\ _lhsIallAttrDecls
                 _lhsIallFields
                 _lhsInts ->
                   (let _lhsOsemPragmasCollect :: PragmaMap
                        _lhsOattrOrderCollect :: AttrOrderMap
                        _lhsOerrors :: (Seq Error)
                        _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                        _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                        _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                        _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                        _constructorSetIcollectedConstructorNames :: (Set ConstructorIdent)
                        _constructorSetIconstructors :: ((Set ConstructorIdent->Set ConstructorIdent))
                        _constructorSetIerrors :: (Seq Error)
                        _rulesIdefinedInsts :: ([Identifier])
                        _rulesIorderDepsCollect :: (Set Dependency)
                        _rulesIpragmaNamesCollect :: ([Identifier])
                        _rulesIruleInfos :: ([RuleInfo])
                        _rulesIsigInfos :: ([SigInfo])
                        _rulesIuniqueInfos :: ([UniqueInfo])
                        -- "Transform.ag"(line 648, column 7)
                        _pragmaNames =
                            Set.fromList _rulesIpragmaNamesCollect
                        -- "Transform.ag"(line 649, column 7)
                        _lhsOsemPragmasCollect =
                            foldr pragmaMapUnion Map.empty [ pragmaMapSingle nt con _pragmaNames
                                                           | (nt, conset, _) <- _coninfo
                                                           , con <- Set.toList conset
                                                           ]
                        -- "Transform.ag"(line 677, column 7)
                        _attrOrders =
                            [ orderMapSingle nt con _rulesIorderDepsCollect
                            | (nt, conset, _) <- _coninfo
                            , con <- Set.toList conset
                            ]
                        -- "Transform.ag"(line 683, column 7)
                        _lhsOattrOrderCollect =
                            foldr orderMapUnion Map.empty _attrOrders
                        -- "Transform.ag"(line 826, column 12)
                        _coninfo =
                            [ (nt, conset, conkeys)
                            | nt  <- Set.toList _lhsInts
                            , let conmap = Map.findWithDefault Map.empty nt _lhsIallFields
                            , let conkeys = Set.fromList (Map.keys conmap)
                            , let conset  = _constructorSetIconstructors conkeys
                            ]
                        -- "Transform.ag"(line 833, column 12)
                        _lhsOerrors =
                            Seq.fromList
                               [ UndefAlt nt con
                               | (nt, conset, conkeys) <- _coninfo
                               , con <- Set.toList (Set.difference conset conkeys)
                               ]
                        -- "Transform.ag"(line 838, column 12)
                        _lhsOcollectedRules =
                            [ (nt,con,r)
                            | (nt, conset, _) <- _coninfo
                            , con <- Set.toList conset
                            , r <- _rulesIruleInfos
                            ]
                        -- "Transform.ag"(line 844, column 12)
                        _lhsOcollectedSigs =
                            [ (nt,con,ts)
                            | (nt, conset, _) <- _coninfo
                            , con <- Set.toList conset
                            , ts <- _rulesIsigInfos
                            ]
                        -- "Transform.ag"(line 851, column 12)
                        _lhsOcollectedInsts =
                            [ (nt,con,_rulesIdefinedInsts)
                            | (nt, conset, _) <- _coninfo
                            , con <- Set.toList conset
                            ]
                        -- "Transform.ag"(line 857, column 12)
                        _lhsOcollectedUniques =
                            [ (nt,con,_rulesIuniqueInfos)
                            | (nt, conset, _) <- _coninfo
                            , con <- Set.toList conset
                            ]
                        ( _constructorSetIcollectedConstructorNames,_constructorSetIconstructors,_constructorSetIerrors) =
                            (constructorSet_ )
                        ( _rulesIdefinedInsts,_rulesIorderDepsCollect,_rulesIpragmaNamesCollect,_rulesIruleInfos,_rulesIsigInfos,_rulesIuniqueInfos) =
                            (rules_ )
                    in  ( _lhsOattrOrderCollect,_lhsOcollectedInsts,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))) )
-- SemAlts -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allAttrDecls         : Map NontermIdent (Attributes, Attributes)
         allFields            : DataTypes
         nts                  : Set NontermIdent
      synthesized attributes:
         attrOrderCollect     : AttrOrderMap
         collectedInsts       : [ (NontermIdent, ConstructorIdent, [Identifier]) ]
         collectedRules       : [ (NontermIdent, ConstructorIdent, RuleInfo)]
         collectedSigs        : [ (NontermIdent, ConstructorIdent, SigInfo) ]
         collectedUniques     : [ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]
         errors               : Seq Error
         semPragmasCollect    : PragmaMap
   alternatives:
      alternative Cons:
         child hd             : SemAlt 
         child tl             : SemAlts 
      alternative Nil:
-}
-- cata
sem_SemAlts :: SemAlts  ->
               T_SemAlts 
sem_SemAlts list  =
    (Prelude.foldr sem_SemAlts_Cons sem_SemAlts_Nil (Prelude.map sem_SemAlt list) )
-- semantic domain
newtype T_SemAlts  = T_SemAlts ((Map NontermIdent (Attributes, Attributes)) ->
                                DataTypes ->
                                (Set NontermIdent) ->
                                ( AttrOrderMap,([ (NontermIdent, ConstructorIdent, [Identifier]) ]),([ (NontermIdent, ConstructorIdent, RuleInfo)]),([ (NontermIdent, ConstructorIdent, SigInfo) ]),([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),(Seq Error),PragmaMap))
data Inh_SemAlts  = Inh_SemAlts {allAttrDecls_Inh_SemAlts :: !(Map NontermIdent (Attributes, Attributes)),allFields_Inh_SemAlts :: !(DataTypes),nts_Inh_SemAlts :: !(Set NontermIdent)}
data Syn_SemAlts  = Syn_SemAlts {attrOrderCollect_Syn_SemAlts :: !(AttrOrderMap),collectedInsts_Syn_SemAlts :: !([ (NontermIdent, ConstructorIdent, [Identifier]) ]),collectedRules_Syn_SemAlts :: !([ (NontermIdent, ConstructorIdent, RuleInfo)]),collectedSigs_Syn_SemAlts :: !([ (NontermIdent, ConstructorIdent, SigInfo) ]),collectedUniques_Syn_SemAlts :: !([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ]),errors_Syn_SemAlts :: !(Seq Error),semPragmasCollect_Syn_SemAlts :: !(PragmaMap)}
wrap_SemAlts :: T_SemAlts  ->
                Inh_SemAlts  ->
                Syn_SemAlts 
wrap_SemAlts (T_SemAlts sem ) (Inh_SemAlts _lhsIallAttrDecls _lhsIallFields _lhsInts )  =
    (let ( _lhsOattrOrderCollect,_lhsOcollectedInsts,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect) =
             (sem _lhsIallAttrDecls _lhsIallFields _lhsInts )
     in  (Syn_SemAlts _lhsOattrOrderCollect _lhsOcollectedInsts _lhsOcollectedRules _lhsOcollectedSigs _lhsOcollectedUniques _lhsOerrors _lhsOsemPragmasCollect ))
sem_SemAlts_Cons :: T_SemAlt  ->
                    T_SemAlts  ->
                    T_SemAlts 
sem_SemAlts_Cons (T_SemAlt hd_ ) (T_SemAlts tl_ )  =
    (T_SemAlts (\ _lhsIallAttrDecls
                  _lhsIallFields
                  _lhsInts ->
                    (let _lhsOattrOrderCollect :: AttrOrderMap
                         _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _lhsOerrors :: (Seq Error)
                         _lhsOsemPragmasCollect :: PragmaMap
                         _hdOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                         _hdOallFields :: DataTypes
                         _hdOnts :: (Set NontermIdent)
                         _tlOallAttrDecls :: (Map NontermIdent (Attributes, Attributes))
                         _tlOallFields :: DataTypes
                         _tlOnts :: (Set NontermIdent)
                         _hdIattrOrderCollect :: AttrOrderMap
                         _hdIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _hdIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _hdIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _hdIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _hdIerrors :: (Seq Error)
                         _hdIsemPragmasCollect :: PragmaMap
                         _tlIattrOrderCollect :: AttrOrderMap
                         _tlIcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _tlIcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _tlIcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _tlIcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _tlIerrors :: (Seq Error)
                         _tlIsemPragmasCollect :: PragmaMap
                         -- use rule "Transform.ag"(line 672, column 55)
                         _lhsOattrOrderCollect =
                             _hdIattrOrderCollect `orderMapUnion` _tlIattrOrderCollect
                         -- use rule "Transform.ag"(line 146, column 31)
                         _lhsOcollectedInsts =
                             _hdIcollectedInsts ++ _tlIcollectedInsts
                         -- use rule "Transform.ag"(line 144, column 31)
                         _lhsOcollectedRules =
                             _hdIcollectedRules ++ _tlIcollectedRules
                         -- use rule "Transform.ag"(line 145, column 31)
                         _lhsOcollectedSigs =
                             _hdIcollectedSigs ++ _tlIcollectedSigs
                         -- use rule "Transform.ag"(line 147, column 31)
                         _lhsOcollectedUniques =
                             _hdIcollectedUniques ++ _tlIcollectedUniques
                         -- use rule "Transform.ag"(line 42, column 19)
                         _lhsOerrors =
                             _hdIerrors Seq.>< _tlIerrors
                         -- use rule "Transform.ag"(line 644, column 56)
                         _lhsOsemPragmasCollect =
                             _hdIsemPragmasCollect `pragmaMapUnion` _tlIsemPragmasCollect
                         -- copy rule (down)
                         _hdOallAttrDecls =
                             _lhsIallAttrDecls
                         -- copy rule (down)
                         _hdOallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _hdOnts =
                             _lhsInts
                         -- copy rule (down)
                         _tlOallAttrDecls =
                             _lhsIallAttrDecls
                         -- copy rule (down)
                         _tlOallFields =
                             _lhsIallFields
                         -- copy rule (down)
                         _tlOnts =
                             _lhsInts
                         ( _hdIattrOrderCollect,_hdIcollectedInsts,_hdIcollectedRules,_hdIcollectedSigs,_hdIcollectedUniques,_hdIerrors,_hdIsemPragmasCollect) =
                             (hd_ _hdOallAttrDecls _hdOallFields _hdOnts )
                         ( _tlIattrOrderCollect,_tlIcollectedInsts,_tlIcollectedRules,_tlIcollectedSigs,_tlIcollectedUniques,_tlIerrors,_tlIsemPragmasCollect) =
                             (tl_ _tlOallAttrDecls _tlOallFields _tlOnts )
                     in  ( _lhsOattrOrderCollect,_lhsOcollectedInsts,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))) )
sem_SemAlts_Nil :: T_SemAlts 
sem_SemAlts_Nil  =
    (T_SemAlts (\ _lhsIallAttrDecls
                  _lhsIallFields
                  _lhsInts ->
                    (let _lhsOattrOrderCollect :: AttrOrderMap
                         _lhsOcollectedInsts :: ([ (NontermIdent, ConstructorIdent, [Identifier]) ])
                         _lhsOcollectedRules :: ([ (NontermIdent, ConstructorIdent, RuleInfo)])
                         _lhsOcollectedSigs :: ([ (NontermIdent, ConstructorIdent, SigInfo) ])
                         _lhsOcollectedUniques :: ([ (NontermIdent, ConstructorIdent, [UniqueInfo]) ])
                         _lhsOerrors :: (Seq Error)
                         _lhsOsemPragmasCollect :: PragmaMap
                         -- use rule "Transform.ag"(line 672, column 55)
                         _lhsOattrOrderCollect =
                             Map.empty
                         -- use rule "Transform.ag"(line 146, column 31)
                         _lhsOcollectedInsts =
                             []
                         -- use rule "Transform.ag"(line 144, column 31)
                         _lhsOcollectedRules =
                             []
                         -- use rule "Transform.ag"(line 145, column 31)
                         _lhsOcollectedSigs =
                             []
                         -- use rule "Transform.ag"(line 147, column 31)
                         _lhsOcollectedUniques =
                             []
                         -- use rule "Transform.ag"(line 42, column 19)
                         _lhsOerrors =
                             Seq.empty
                         -- use rule "Transform.ag"(line 644, column 56)
                         _lhsOsemPragmasCollect =
                             Map.empty
                     in  ( _lhsOattrOrderCollect,_lhsOcollectedInsts,_lhsOcollectedRules,_lhsOcollectedSigs,_lhsOcollectedUniques,_lhsOerrors,_lhsOsemPragmasCollect))) )
-- SemDef ------------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         definedInsts         : [Identifier]
         orderDepsCollect     : Set Dependency
         pragmaNamesCollect   : [Identifier]
         ruleInfos            : [RuleInfo]
         sigInfos             : [SigInfo]
         uniqueInfos          : [UniqueInfo]
   alternatives:
      alternative AttrOrderBefore:
         child before         : {[(Identifier,Identifier)]}
         child after          : {[(Identifier,Identifier)]}
         visit 0:
            local dependency  : _
      alternative Def:
         child pattern        : Pattern 
         child rhs            : {Expression}
         child owrt           : {Bool}
      alternative SemPragma:
         child names          : {[NontermIdent]}
      alternative TypeDef:
         child ident          : {Identifier}
         child tp             : {Type}
      alternative UniqueDef:
         child ident          : {Identifier}
         child ref            : {Identifier}
-}
-- cata
sem_SemDef :: SemDef  ->
              T_SemDef 
sem_SemDef (AttrOrderBefore _before _after )  =
    (sem_SemDef_AttrOrderBefore _before _after )
sem_SemDef (Def _pattern _rhs _owrt )  =
    (sem_SemDef_Def (sem_Pattern _pattern ) _rhs _owrt )
sem_SemDef (SemPragma _names )  =
    (sem_SemDef_SemPragma _names )
sem_SemDef (TypeDef _ident _tp )  =
    (sem_SemDef_TypeDef _ident _tp )
sem_SemDef (UniqueDef _ident _ref )  =
    (sem_SemDef_UniqueDef _ident _ref )
-- semantic domain
newtype T_SemDef  = T_SemDef (( ([Identifier]),(Set Dependency),([Identifier]),([RuleInfo]),([SigInfo]),([UniqueInfo])))
data Inh_SemDef  = Inh_SemDef {}
data Syn_SemDef  = Syn_SemDef {definedInsts_Syn_SemDef :: !([Identifier]),orderDepsCollect_Syn_SemDef :: !(Set Dependency),pragmaNamesCollect_Syn_SemDef :: !([Identifier]),ruleInfos_Syn_SemDef :: !([RuleInfo]),sigInfos_Syn_SemDef :: !([SigInfo]),uniqueInfos_Syn_SemDef :: !([UniqueInfo])}
wrap_SemDef :: T_SemDef  ->
               Inh_SemDef  ->
               Syn_SemDef 
wrap_SemDef (T_SemDef sem ) (Inh_SemDef )  =
    (let ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos) =
             (sem )
     in  (Syn_SemDef _lhsOdefinedInsts _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos ))
sem_SemDef_AttrOrderBefore :: ([(Identifier,Identifier)]) ->
                              ([(Identifier,Identifier)]) ->
                              T_SemDef 
sem_SemDef_AttrOrderBefore before_ after_  =
    (T_SemDef (let _lhsOorderDepsCollect :: (Set Dependency)
                   _lhsOdefinedInsts :: ([Identifier])
                   _lhsOpragmaNamesCollect :: ([Identifier])
                   _lhsOruleInfos :: ([RuleInfo])
                   _lhsOsigInfos :: ([SigInfo])
                   _lhsOuniqueInfos :: ([UniqueInfo])
                   -- "Transform.ag"(line 689, column 7)
                   _dependency =
                       [ Dependency b a | b <- before_, a <- after_ ]
                   -- "Transform.ag"(line 690, column 7)
                   _lhsOorderDepsCollect =
                       Set.fromList _dependency
                   -- use rule "Transform.ag"(line 874, column 55)
                   _lhsOdefinedInsts =
                       []
                   -- use rule "Transform.ag"(line 654, column 46)
                   _lhsOpragmaNamesCollect =
                       []
                   -- use rule "Transform.ag"(line 819, column 39)
                   _lhsOruleInfos =
                       []
                   -- use rule "Transform.ag"(line 820, column 39)
                   _lhsOsigInfos =
                       []
                   -- use rule "Transform.ag"(line 821, column 39)
                   _lhsOuniqueInfos =
                       []
               in  ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos)) )
sem_SemDef_Def :: T_Pattern  ->
                  Expression ->
                  Bool ->
                  T_SemDef 
sem_SemDef_Def (T_Pattern pattern_ ) rhs_ owrt_  =
    (T_SemDef (let _lhsOruleInfos :: ([RuleInfo])
                   _lhsOdefinedInsts :: ([Identifier])
                   _lhsOorderDepsCollect :: (Set Dependency)
                   _lhsOpragmaNamesCollect :: ([Identifier])
                   _lhsOsigInfos :: ([SigInfo])
                   _lhsOuniqueInfos :: ([UniqueInfo])
                   _patternIcopy :: Pattern
                   _patternIdefinedAttrs :: ([AttrName])
                   _patternIdefinedInsts :: ([Identifier])
                   _patternIpatunder :: ([AttrName]->Pattern)
                   _patternIstpos :: Pos
                   -- "Transform.ag"(line 865, column 10)
                   _lhsOruleInfos =
                       [ (_patternIpatunder, rhs_, _patternIdefinedAttrs, owrt_, show _patternIstpos) ]
                   -- use rule "Transform.ag"(line 874, column 55)
                   _lhsOdefinedInsts =
                       _patternIdefinedInsts
                   -- use rule "Transform.ag"(line 685, column 44)
                   _lhsOorderDepsCollect =
                       Set.empty
                   -- use rule "Transform.ag"(line 654, column 46)
                   _lhsOpragmaNamesCollect =
                       []
                   -- use rule "Transform.ag"(line 820, column 39)
                   _lhsOsigInfos =
                       []
                   -- use rule "Transform.ag"(line 821, column 39)
                   _lhsOuniqueInfos =
                       []
                   ( _patternIcopy,_patternIdefinedAttrs,_patternIdefinedInsts,_patternIpatunder,_patternIstpos) =
                       (pattern_ )
               in  ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos)) )
sem_SemDef_SemPragma :: ([NontermIdent]) ->
                        T_SemDef 
sem_SemDef_SemPragma names_  =
    (T_SemDef (let _lhsOpragmaNamesCollect :: ([Identifier])
                   _lhsOdefinedInsts :: ([Identifier])
                   _lhsOorderDepsCollect :: (Set Dependency)
                   _lhsOruleInfos :: ([RuleInfo])
                   _lhsOsigInfos :: ([SigInfo])
                   _lhsOuniqueInfos :: ([UniqueInfo])
                   -- "Transform.ag"(line 658, column 7)
                   _lhsOpragmaNamesCollect =
                       names_
                   -- use rule "Transform.ag"(line 874, column 55)
                   _lhsOdefinedInsts =
                       []
                   -- use rule "Transform.ag"(line 685, column 44)
                   _lhsOorderDepsCollect =
                       Set.empty
                   -- use rule "Transform.ag"(line 819, column 39)
                   _lhsOruleInfos =
                       []
                   -- use rule "Transform.ag"(line 820, column 39)
                   _lhsOsigInfos =
                       []
                   -- use rule "Transform.ag"(line 821, column 39)
                   _lhsOuniqueInfos =
                       []
               in  ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos)) )
sem_SemDef_TypeDef :: Identifier ->
                      Type ->
                      T_SemDef 
sem_SemDef_TypeDef ident_ tp_  =
    (T_SemDef (let _lhsOsigInfos :: ([SigInfo])
                   _lhsOdefinedInsts :: ([Identifier])
                   _lhsOorderDepsCollect :: (Set Dependency)
                   _lhsOpragmaNamesCollect :: ([Identifier])
                   _lhsOruleInfos :: ([RuleInfo])
                   _lhsOuniqueInfos :: ([UniqueInfo])
                   -- "Transform.ag"(line 868, column 14)
                   _lhsOsigInfos =
                       [ (ident_, tp_) ]
                   -- use rule "Transform.ag"(line 874, column 55)
                   _lhsOdefinedInsts =
                       []
                   -- use rule "Transform.ag"(line 685, column 44)
                   _lhsOorderDepsCollect =
                       Set.empty
                   -- use rule "Transform.ag"(line 654, column 46)
                   _lhsOpragmaNamesCollect =
                       []
                   -- use rule "Transform.ag"(line 819, column 39)
                   _lhsOruleInfos =
                       []
                   -- use rule "Transform.ag"(line 821, column 39)
                   _lhsOuniqueInfos =
                       []
               in  ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos)) )
sem_SemDef_UniqueDef :: Identifier ->
                        Identifier ->
                        T_SemDef 
sem_SemDef_UniqueDef ident_ ref_  =
    (T_SemDef (let _lhsOuniqueInfos :: ([UniqueInfo])
                   _lhsOdefinedInsts :: ([Identifier])
                   _lhsOorderDepsCollect :: (Set Dependency)
                   _lhsOpragmaNamesCollect :: ([Identifier])
                   _lhsOruleInfos :: ([RuleInfo])
                   _lhsOsigInfos :: ([SigInfo])
                   -- "Transform.ag"(line 871, column 16)
                   _lhsOuniqueInfos =
                       [ (ident_, ref_) ]
                   -- use rule "Transform.ag"(line 874, column 55)
                   _lhsOdefinedInsts =
                       []
                   -- use rule "Transform.ag"(line 685, column 44)
                   _lhsOorderDepsCollect =
                       Set.empty
                   -- use rule "Transform.ag"(line 654, column 46)
                   _lhsOpragmaNamesCollect =
                       []
                   -- use rule "Transform.ag"(line 819, column 39)
                   _lhsOruleInfos =
                       []
                   -- use rule "Transform.ag"(line 820, column 39)
                   _lhsOsigInfos =
                       []
               in  ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos)) )
-- SemDefs -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         definedInsts         : [Identifier]
         orderDepsCollect     : Set Dependency
         pragmaNamesCollect   : [Identifier]
         ruleInfos            : [RuleInfo]
         sigInfos             : [SigInfo]
         uniqueInfos          : [UniqueInfo]
   alternatives:
      alternative Cons:
         child hd             : SemDef 
         child tl             : SemDefs 
      alternative Nil:
-}
-- cata
sem_SemDefs :: SemDefs  ->
               T_SemDefs 
sem_SemDefs list  =
    (Prelude.foldr sem_SemDefs_Cons sem_SemDefs_Nil (Prelude.map sem_SemDef list) )
-- semantic domain
newtype T_SemDefs  = T_SemDefs (( ([Identifier]),(Set Dependency),([Identifier]),([RuleInfo]),([SigInfo]),([UniqueInfo])))
data Inh_SemDefs  = Inh_SemDefs {}
data Syn_SemDefs  = Syn_SemDefs {definedInsts_Syn_SemDefs :: !([Identifier]),orderDepsCollect_Syn_SemDefs :: !(Set Dependency),pragmaNamesCollect_Syn_SemDefs :: !([Identifier]),ruleInfos_Syn_SemDefs :: !([RuleInfo]),sigInfos_Syn_SemDefs :: !([SigInfo]),uniqueInfos_Syn_SemDefs :: !([UniqueInfo])}
wrap_SemDefs :: T_SemDefs  ->
                Inh_SemDefs  ->
                Syn_SemDefs 
wrap_SemDefs (T_SemDefs sem ) (Inh_SemDefs )  =
    (let ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos) =
             (sem )
     in  (Syn_SemDefs _lhsOdefinedInsts _lhsOorderDepsCollect _lhsOpragmaNamesCollect _lhsOruleInfos _lhsOsigInfos _lhsOuniqueInfos ))
sem_SemDefs_Cons :: T_SemDef  ->
                    T_SemDefs  ->
                    T_SemDefs 
sem_SemDefs_Cons (T_SemDef hd_ ) (T_SemDefs tl_ )  =
    (T_SemDefs (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOorderDepsCollect :: (Set Dependency)
                    _lhsOpragmaNamesCollect :: ([Identifier])
                    _lhsOruleInfos :: ([RuleInfo])
                    _lhsOsigInfos :: ([SigInfo])
                    _lhsOuniqueInfos :: ([UniqueInfo])
                    _hdIdefinedInsts :: ([Identifier])
                    _hdIorderDepsCollect :: (Set Dependency)
                    _hdIpragmaNamesCollect :: ([Identifier])
                    _hdIruleInfos :: ([RuleInfo])
                    _hdIsigInfos :: ([SigInfo])
                    _hdIuniqueInfos :: ([UniqueInfo])
                    _tlIdefinedInsts :: ([Identifier])
                    _tlIorderDepsCollect :: (Set Dependency)
                    _tlIpragmaNamesCollect :: ([Identifier])
                    _tlIruleInfos :: ([RuleInfo])
                    _tlIsigInfos :: ([SigInfo])
                    _tlIuniqueInfos :: ([UniqueInfo])
                    -- use rule "Transform.ag"(line 874, column 55)
                    _lhsOdefinedInsts =
                        _hdIdefinedInsts ++ _tlIdefinedInsts
                    -- use rule "Transform.ag"(line 685, column 44)
                    _lhsOorderDepsCollect =
                        _hdIorderDepsCollect `Set.union` _tlIorderDepsCollect
                    -- use rule "Transform.ag"(line 654, column 46)
                    _lhsOpragmaNamesCollect =
                        _hdIpragmaNamesCollect ++ _tlIpragmaNamesCollect
                    -- use rule "Transform.ag"(line 819, column 39)
                    _lhsOruleInfos =
                        _hdIruleInfos ++ _tlIruleInfos
                    -- use rule "Transform.ag"(line 820, column 39)
                    _lhsOsigInfos =
                        _hdIsigInfos ++ _tlIsigInfos
                    -- use rule "Transform.ag"(line 821, column 39)
                    _lhsOuniqueInfos =
                        _hdIuniqueInfos ++ _tlIuniqueInfos
                    ( _hdIdefinedInsts,_hdIorderDepsCollect,_hdIpragmaNamesCollect,_hdIruleInfos,_hdIsigInfos,_hdIuniqueInfos) =
                        (hd_ )
                    ( _tlIdefinedInsts,_tlIorderDepsCollect,_tlIpragmaNamesCollect,_tlIruleInfos,_tlIsigInfos,_tlIuniqueInfos) =
                        (tl_ )
                in  ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos)) )
sem_SemDefs_Nil :: T_SemDefs 
sem_SemDefs_Nil  =
    (T_SemDefs (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOorderDepsCollect :: (Set Dependency)
                    _lhsOpragmaNamesCollect :: ([Identifier])
                    _lhsOruleInfos :: ([RuleInfo])
                    _lhsOsigInfos :: ([SigInfo])
                    _lhsOuniqueInfos :: ([UniqueInfo])
                    -- use rule "Transform.ag"(line 874, column 55)
                    _lhsOdefinedInsts =
                        []
                    -- use rule "Transform.ag"(line 685, column 44)
                    _lhsOorderDepsCollect =
                        Set.empty
                    -- use rule "Transform.ag"(line 654, column 46)
                    _lhsOpragmaNamesCollect =
                        []
                    -- use rule "Transform.ag"(line 819, column 39)
                    _lhsOruleInfos =
                        []
                    -- use rule "Transform.ag"(line 820, column 39)
                    _lhsOsigInfos =
                        []
                    -- use rule "Transform.ag"(line 821, column 39)
                    _lhsOuniqueInfos =
                        []
                in  ( _lhsOdefinedInsts,_lhsOorderDepsCollect,_lhsOpragmaNamesCollect,_lhsOruleInfos,_lhsOsigInfos,_lhsOuniqueInfos)) )