

-- UUAGC 0.9.10 (GenerateCode.ag)
module GenerateCode where

import CommonTypes
import SequentialTypes
import Code hiding (Type)
import qualified Code
import Options
import CodeSyntax
import ErrorMessages
import GrammarInfo
import DeclBlocks

import qualified Data.Map as Map
import Data.Map(Map) 
import qualified Data.Set as Set
import Data.Set(Set) 
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import UU.Scanner.Position
import TokenDef
import HsToken
import HsTokenScanner

import Data.List(partition,intersperse,intersect,(\\))
import Maybe(fromJust,isJust)


import Patterns
import CommonTypes
import Data.Map(Map)
import Data.Set(Set)


-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)


import Code (Decl,Expr)

-- remove possible @v references in the types of a data type.
cleanupArg :: String -> String
cleanupArg s
  = case idEvalType (SimpleType s) of
      SimpleType s' -> s'


appContext :: ContextMap -> NontermIdent -> Code.Type -> Code.Type
appContext mp nt tp
  = maybe tp (\ctx -> CtxApp (map (\(n,ns) -> (getName n, ns)) ctx) tp) $ Map.lookup nt mp


substSelf nt tp = case tp of
                    NT t tps | t == _SELF -> Haskell (getName nt ++ " " ++ unwords tps)
                    _                     -> tp


mkLambdaArg :: String -> Maybe Code.Type -> Expr
mkLambdaArg nm Nothing = SimpleExpr nm
mkLambdaArg nm (Just tp) = TypedExpr (SimpleExpr nm) tp

mkLambda :: Exprs -> Expr -> Expr
mkLambda [] e = e
mkLambda xs e = Lambda xs e

typeAppStrs nm params = TypeApp (SimpleType nm) (map SimpleType params)


-- dead code - replaced by mkPartitionedFunction
{-
localCpsToExpr :: Bool -> [Decl] -> DeclBlocks -> Expr
localCpsToExpr o_case nextVisitDecl
  = rec
  where
    rec cps
      = case cps of
          DeclTerminator decls expr
            -> mkLet o_case (decls ++ nextVisitDecl) expr
          DeclBlock decls (Decl lhs rhs _ _) remainder
            -> let inDecls = [ PragmaDecl ("NOINLINE " ++ internalCpsName)
                             , Decl (Fun internalCpsName [SimpleExpr internalCpsVarName])
                                    (mkLet o_case [Decl lhs (SimpleExpr internalCpsVarName) Set.empty Set.empty] (rec remainder))
                                    Set.empty Set.empty
                             ]
                   inBody = App "head" [App "(:)" [App internalCpsName [rhs], App "(:)" [App internalCpsName [rhs], SimpleExpr "[]"]]]
                   outerBody = Let inDecls inBody
                   block = mkLet o_case decls outerBody
               in block

internalCpsName :: String
internalCpsName = "internalCps_"

internalCpsVarName :: String
internalCpsVarName = "internalCps_var_"
-}

mkPartitionedFunction :: String -> Bool -> [Decl] -> [String] -> DeclBlocks -> ([Decl], Expr)
mkPartitionedFunction prefix optCase nextVisitDecls lastExprVars cpsTree
  = let inh = Inh_DeclBlocksRoot { prefix_Inh_DeclBlocksRoot = prefix
                                 , optCase_Inh_DeclBlocksRoot = optCase
                                 , nextVisitDecls_Inh_DeclBlocksRoot = nextVisitDecls
                                 , lastExprVars_Inh_DeclBlocksRoot = lastExprVars
                                 }
        sem = sem_DeclBlocksRoot (DeclBlocksRoot cpsTree)
        syn = wrap_DeclBlocksRoot sem inh
    in (lambdas_Syn_DeclBlocksRoot syn, firstCall_Syn_DeclBlocksRoot syn)


freevars :: [String] -> [Decl] -> [String]
freevars additional decls
  = Set.toList (allused `Set.difference` alldefined)
  where
    allused = Set.unions (Set.fromList additional : map usedvars decls)
    alldefined = Set.unions (map definedvars decls)
  
    usedvars (Decl _ _ _ uses) = uses
    usedvars _                 = Set.empty
    
    definedvars (Decl _ _ defs _) = defs
    definedvars _                 = Set.empty

mkBlockLambda :: Bool -> String -> [String] -> [Decl] -> Expr -> Decl
mkBlockLambda optCase name args decls expr
  = Decl lhs rhs Set.empty Set.empty
  where
    lhs = Fun name (map SimpleExpr args)
    rhs = mkLet optCase decls expr


evalType :: (String -> String) -> Code.Type -> Code.Type
evalType replf t
  = chase t
  where
    chase t
      = case t of
          Arr l r              -> Arr (chase l) (chase r)
          TypeApp f as         -> TypeApp (chase f) (map chase as)
          TupleType tps        -> TupleType (map chase tps)
          UnboxedTupleType tps -> UnboxedTupleType (map chase tps)
          Code.List tp         -> Code.List (chase tp)
          SimpleType txt       -> let tks  = lexTokens (initPos txt) txt
                                      tks' = map replaceTok tks
                                      txt' = unlines . showTokens . tokensToStrings $ tks'
                                  in SimpleType txt'

    replaceTok t
      = case t of
          AGLocal v p _ -> HsToken (replf $ getName v) p
          _             -> t

idEvalType :: Code.Type -> Code.Type
idEvalType = evalType id


makeLocalComment :: Int -> String -> Identifier -> Maybe Type -> String
makeLocalComment width what  name tp = let  x = getName name
                                            y = maybe "_" (\t -> case t of (NT nt tps) -> getName nt ++ " " ++ unwords tps; Haskell t -> '{':t++"}") tp
                                       in   ( what ++ " " ++ x ++ replicate ((width - length x) `max` 0) ' ' ++ " : " ++ y )



-- Lets or nested Cases?
mkLet :: Bool -> Decls -> Expr -> Expr
mkLet False decls body = Let decls body
mkLet True decls body = foldr oneCase body decls

oneCase :: Decl -> Expr -> Expr
oneCase (Decl left rhs _ _) exp = Case rhs [CaseAlt left exp]
oneCase _                   exp = exp

-- Gives the name of the visit function
funname field 0  = show field ++ "_"
funname field nr = show field ++ "_" ++ show nr

-- Gives the name of a semantic function
seqSemname :: String -> NontermIdent -> ConstructorIdent -> Int -> String
seqSemname pre nt con  0 = semname pre nt con
seqSemname pre nt con nr = semname pre nt con ++ "_" ++ show nr

-- Gives the name of a type
typeName :: NontermIdent -> Int -> String
typeName nt 0 = "T_" ++ show nt
typeName nt n = "T_" ++ show nt ++ "_" ++ show n

ntOfVisit :: NontermIdent -> Int -> NontermIdent
ntOfVisit nt 0 = nt
ntOfVisit nt n = Ident (show nt ++ "_" ++ show n) (getPos nt)

-- Gives the name of a visit function
visitname  ::  String -> NontermIdent -> Int -> String
visitname pre nt n =  pre ++ getName nt ++ "_" ++ show n
-- CGrammar ----------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attributes:
         errors               : Seq Error
         output               : Program
   alternatives:
      alternative CGrammar:
         child typeSyns       : {TypeSyns}
         child derivings      : {Derivings}
         child wrappers       : {Set NontermIdent}
         child nonts          : CNonterminals 
         child pragmas        : {PragmaMap}
         child paramMap       : {ParamMap}
         child contextMap     : {ContextMap}
         visit 0:
            local unfoldSemDom : _
-}
-- cata
sem_CGrammar :: CGrammar  ->
                T_CGrammar 
sem_CGrammar (CGrammar _typeSyns _derivings _wrappers _nonts _pragmas _paramMap _contextMap )  =
    (sem_CGrammar_CGrammar _typeSyns _derivings _wrappers (sem_CNonterminals _nonts ) _pragmas _paramMap _contextMap )
-- semantic domain
newtype T_CGrammar  = T_CGrammar (Options ->
                                  ( (Seq Error),Program))
data Inh_CGrammar  = Inh_CGrammar {options_Inh_CGrammar :: !(Options)}
data Syn_CGrammar  = Syn_CGrammar {errors_Syn_CGrammar :: !(Seq Error),output_Syn_CGrammar :: !(Program)}
wrap_CGrammar :: T_CGrammar  ->
                 Inh_CGrammar  ->
                 Syn_CGrammar 
wrap_CGrammar (T_CGrammar sem ) (Inh_CGrammar _lhsIoptions )  =
    (let ( _lhsOerrors,_lhsOoutput) =
             (sem _lhsIoptions )
     in  (Syn_CGrammar _lhsOerrors _lhsOoutput ))
sem_CGrammar_CGrammar :: TypeSyns ->
                         Derivings ->
                         (Set NontermIdent) ->
                         T_CNonterminals  ->
                         PragmaMap ->
                         ParamMap ->
                         ContextMap ->
                         T_CGrammar 
sem_CGrammar_CGrammar typeSyns_ derivings_ wrappers_ (T_CNonterminals nonts_ ) pragmas_ paramMap_ contextMap_  =
    (T_CGrammar (\ _lhsIoptions ->
                     (let _nontsOo_sig :: Bool
                          _nontsOo_cata :: Bool
                          _nontsOo_sem :: Bool
                          _nontsOo_newtypes :: Bool
                          _nontsOo_unbox :: Bool
                          _nontsOo_case :: Bool
                          _nontsOo_pretty :: Bool
                          _nontsOo_rename :: Bool
                          _nontsOo_strictwrap :: Bool
                          _nontsOo_splitsems :: Bool
                          _nontsOo_data :: (Maybe Bool)
                          _nontsOprefix :: String
                          _nontsOo_traces :: Bool
                          _nontsOo_costcentre :: Bool
                          _nontsOo_linePragmas :: Bool
                          _nontsOallPragmas :: PragmaMap
                          _nontsOparamMap :: ParamMap
                          _nontsOcontextMap :: ContextMap
                          _nontsOallNts :: (Set NontermIdent)
                          _nontsOwith_sig :: Bool
                          _nontsOerrors :: (Seq Error)
                          _lhsOoutput :: Program
                          _nontsOtypeSyns :: TypeSyns
                          _nontsOderivings :: Derivings
                          _nontsOwrappers :: (Set NontermIdent)
                          _lhsOerrors :: (Seq Error)
                          _nontsOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                          _nontsIallTpsFound :: Bool
                          _nontsIchunks :: Chunks
                          _nontsIerrors :: (Seq Error)
                          _nontsIgathNts :: (Set NontermIdent)
                          _nontsIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_sig =
                              typeSigs      _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_cata =
                              folds         _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_sem =
                              semfuns       _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_newtypes =
                              newtypes      _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_unbox =
                              unbox         _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_case =
                              cases         _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_pretty =
                              attrInfo      _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_rename =
                              rename        _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_strictwrap =
                              strictWrap    _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_splitsems =
                              splitSems     _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_data =
                              if dataTypes _lhsIoptions then Just (strictData _lhsIoptions) else Nothing
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOprefix =
                              prefix        _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_traces =
                              genTraces     _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_costcentre =
                              genCostCentres _lhsIoptions
                          -- "GenerateCode.ag"(line 50, column 17)
                          _nontsOo_linePragmas =
                              genLinePragmas _lhsIoptions
                          -- "GenerateCode.ag"(line 69, column 15)
                          _nontsOallPragmas =
                              pragmas_
                          -- "GenerateCode.ag"(line 91, column 14)
                          _nontsOparamMap =
                              paramMap_
                          -- "GenerateCode.ag"(line 112, column 7)
                          _nontsOcontextMap =
                              contextMap_
                          -- "GenerateCode.ag"(line 124, column 7)
                          _nontsOallNts =
                              _nontsIgathNts
                          -- "GenerateCode.ag"(line 621, column 7)
                          _unfoldSemDom =
                              \nt nr repl ->
                               let (params, tp) = Map.findWithDefault (error ("No such semantic domain: " ++ show nt)) (nt, nr) _nontsIsemDomUnfoldGath
                                   replMap = Map.fromList (zip params repl)
                                   replace k = Map.findWithDefault ('@':k) k replMap
                               in evalType replace tp
                          -- "GenerateCode.ag"(line 714, column 14)
                          _nontsOwith_sig =
                              typeSigs _lhsIoptions && _nontsIallTpsFound
                          -- "GenerateCode.ag"(line 721, column 15)
                          _nontsOerrors =
                              Seq.empty
                          -- "GenerateCode.ag"(line 785, column 17)
                          _lhsOoutput =
                              Program _nontsIchunks
                          -- "GenerateCode.ag"(line 838, column 14)
                          _nontsOtypeSyns =
                              typeSyns_
                          -- "GenerateCode.ag"(line 838, column 14)
                          _nontsOderivings =
                              derivings_
                          -- "GenerateCode.ag"(line 838, column 14)
                          _nontsOwrappers =
                              wrappers_
                          -- copy rule (up)
                          _lhsOerrors =
                              _nontsIerrors
                          -- copy rule (from local)
                          _nontsOunfoldSemDom =
                              _unfoldSemDom
                          ( _nontsIallTpsFound,_nontsIchunks,_nontsIerrors,_nontsIgathNts,_nontsIsemDomUnfoldGath) =
                              (nonts_ _nontsOallNts _nontsOallPragmas _nontsOcontextMap _nontsOderivings _nontsOerrors _nontsOo_case _nontsOo_cata _nontsOo_costcentre _nontsOo_data _nontsOo_linePragmas _nontsOo_newtypes _nontsOo_pretty _nontsOo_rename _nontsOo_sem _nontsOo_sig _nontsOo_splitsems _nontsOo_strictwrap _nontsOo_traces _nontsOo_unbox _nontsOparamMap _nontsOprefix _nontsOtypeSyns _nontsOunfoldSemDom _nontsOwith_sig _nontsOwrappers )
                      in  ( _lhsOerrors,_lhsOoutput))) )
-- CInterface --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inh                  : Attributes
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
      synthesized attributes:
         comments             : [String]
         semDom               : [Decl]
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
         wrapDecls            : Decls
   alternatives:
      alternative CInterface:
         child seg            : CSegments 
-}
-- cata
sem_CInterface :: CInterface  ->
                  T_CInterface 
sem_CInterface (CInterface _seg )  =
    (sem_CInterface_CInterface (sem_CSegments _seg ) )
-- semantic domain
newtype T_CInterface  = T_CInterface (Attributes ->
                                      NontermIdent ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      (Maybe Bool) ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      Bool ->
                                      ParamMap ->
                                      String ->
                                      Attributes ->
                                      ( ([String]),([Decl]),(Map (NontermIdent, Int) ([String], Code.Type)),Decls))
data Inh_CInterface  = Inh_CInterface {inh_Inh_CInterface :: !(Attributes),nt_Inh_CInterface :: !(NontermIdent),o_case_Inh_CInterface :: !(Bool),o_cata_Inh_CInterface :: !(Bool),o_costcentre_Inh_CInterface :: !(Bool),o_data_Inh_CInterface :: !(Maybe Bool),o_linePragmas_Inh_CInterface :: !(Bool),o_newtypes_Inh_CInterface :: !(Bool),o_pretty_Inh_CInterface :: !(Bool),o_rename_Inh_CInterface :: !(Bool),o_sem_Inh_CInterface :: !(Bool),o_sig_Inh_CInterface :: !(Bool),o_splitsems_Inh_CInterface :: !(Bool),o_strictwrap_Inh_CInterface :: !(Bool),o_traces_Inh_CInterface :: !(Bool),o_unbox_Inh_CInterface :: !(Bool),paramMap_Inh_CInterface :: !(ParamMap),prefix_Inh_CInterface :: !(String),syn_Inh_CInterface :: !(Attributes)}
data Syn_CInterface  = Syn_CInterface {comments_Syn_CInterface :: !([String]),semDom_Syn_CInterface :: !([Decl]),semDomUnfoldGath_Syn_CInterface :: !(Map (NontermIdent, Int) ([String], Code.Type)),wrapDecls_Syn_CInterface :: !(Decls)}
wrap_CInterface :: T_CInterface  ->
                   Inh_CInterface  ->
                   Syn_CInterface 
wrap_CInterface (T_CInterface sem ) (Inh_CInterface _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn )  =
    (let ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls) =
             (sem _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn )
     in  (Syn_CInterface _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls ))
sem_CInterface_CInterface :: T_CSegments  ->
                             T_CInterface 
sem_CInterface_CInterface (T_CSegments seg_ )  =
    (T_CInterface (\ _lhsIinh
                     _lhsInt
                     _lhsIo_case
                     _lhsIo_cata
                     _lhsIo_costcentre
                     _lhsIo_data
                     _lhsIo_linePragmas
                     _lhsIo_newtypes
                     _lhsIo_pretty
                     _lhsIo_rename
                     _lhsIo_sem
                     _lhsIo_sig
                     _lhsIo_splitsems
                     _lhsIo_strictwrap
                     _lhsIo_traces
                     _lhsIo_unbox
                     _lhsIparamMap
                     _lhsIprefix
                     _lhsIsyn ->
                       (let _segOnr :: Int
                            _lhsOsemDom :: ([Decl])
                            _lhsOcomments :: ([String])
                            _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                            _lhsOwrapDecls :: Decls
                            _segOinh :: Attributes
                            _segOnt :: NontermIdent
                            _segOo_case :: Bool
                            _segOo_cata :: Bool
                            _segOo_costcentre :: Bool
                            _segOo_data :: (Maybe Bool)
                            _segOo_linePragmas :: Bool
                            _segOo_newtypes :: Bool
                            _segOo_pretty :: Bool
                            _segOo_rename :: Bool
                            _segOo_sem :: Bool
                            _segOo_sig :: Bool
                            _segOo_splitsems :: Bool
                            _segOo_strictwrap :: Bool
                            _segOo_traces :: Bool
                            _segOo_unbox :: Bool
                            _segOparamMap :: ParamMap
                            _segOprefix :: String
                            _segOsyn :: Attributes
                            _segIcomments :: ([String])
                            _segIisNil :: Bool
                            _segIsemDom :: ([Decl])
                            _segIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                            _segIwrapDecls :: Decls
                            -- "GenerateCode.ag"(line 218, column 17)
                            _segOnr =
                                0
                            -- "GenerateCode.ag"(line 594, column 18)
                            _lhsOsemDom =
                                Comment "semantic domain" : _segIsemDom
                            -- use rule "GenerateCode.ag"(line 738, column 52)
                            _lhsOcomments =
                                _segIcomments
                            -- use rule "GenerateCode.ag"(line 611, column 86)
                            _lhsOsemDomUnfoldGath =
                                _segIsemDomUnfoldGath
                            -- use rule "GenerateCode.ag"(line 687, column 52)
                            _lhsOwrapDecls =
                                _segIwrapDecls
                            -- copy rule (down)
                            _segOinh =
                                _lhsIinh
                            -- copy rule (down)
                            _segOnt =
                                _lhsInt
                            -- copy rule (down)
                            _segOo_case =
                                _lhsIo_case
                            -- copy rule (down)
                            _segOo_cata =
                                _lhsIo_cata
                            -- copy rule (down)
                            _segOo_costcentre =
                                _lhsIo_costcentre
                            -- copy rule (down)
                            _segOo_data =
                                _lhsIo_data
                            -- copy rule (down)
                            _segOo_linePragmas =
                                _lhsIo_linePragmas
                            -- copy rule (down)
                            _segOo_newtypes =
                                _lhsIo_newtypes
                            -- copy rule (down)
                            _segOo_pretty =
                                _lhsIo_pretty
                            -- copy rule (down)
                            _segOo_rename =
                                _lhsIo_rename
                            -- copy rule (down)
                            _segOo_sem =
                                _lhsIo_sem
                            -- copy rule (down)
                            _segOo_sig =
                                _lhsIo_sig
                            -- copy rule (down)
                            _segOo_splitsems =
                                _lhsIo_splitsems
                            -- copy rule (down)
                            _segOo_strictwrap =
                                _lhsIo_strictwrap
                            -- copy rule (down)
                            _segOo_traces =
                                _lhsIo_traces
                            -- copy rule (down)
                            _segOo_unbox =
                                _lhsIo_unbox
                            -- copy rule (down)
                            _segOparamMap =
                                _lhsIparamMap
                            -- copy rule (down)
                            _segOprefix =
                                _lhsIprefix
                            -- copy rule (down)
                            _segOsyn =
                                _lhsIsyn
                            ( _segIcomments,_segIisNil,_segIsemDom,_segIsemDomUnfoldGath,_segIwrapDecls) =
                                (seg_ _segOinh _segOnr _segOnt _segOo_case _segOo_cata _segOo_costcentre _segOo_data _segOo_linePragmas _segOo_newtypes _segOo_pretty _segOo_rename _segOo_sem _segOo_sig _segOo_splitsems _segOo_strictwrap _segOo_traces _segOo_unbox _segOparamMap _segOprefix _segOsyn )
                        in  ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))) )
-- CNonterminal ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         contextMap           : ContextMap
         derivings            : Derivings
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramMap             : ParamMap
         prefix               : String
         typeSyns             : TypeSyns
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      chained attribute:
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         chunks               : Chunks
         gathNts              : Set NontermIdent
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
   alternatives:
      alternative CNonterminal:
         child nt             : {NontermIdent}
         child params         : {[Identifier]}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child prods          : CProductions 
         child inter          : CInterface 
         visit 0:
            local _tup1       : {(Attributes,Attributes,NontermIdent)}
            local _tup2       : {(Attributes,Attributes,NontermIdent)}
            local semWrapper  : _
            local comment     : _
            local dataDef     : _
            local cataFun     : _
-}
-- cata
sem_CNonterminal :: CNonterminal  ->
                    T_CNonterminal 
sem_CNonterminal (CNonterminal _nt _params _inh _syn _prods _inter )  =
    (sem_CNonterminal_CNonterminal _nt _params _inh _syn (sem_CProductions _prods ) (sem_CInterface _inter ) )
-- semantic domain
newtype T_CNonterminal  = T_CNonterminal ((Set NontermIdent) ->
                                          PragmaMap ->
                                          ContextMap ->
                                          Derivings ->
                                          (Seq Error) ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          (Maybe Bool) ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          ParamMap ->
                                          String ->
                                          TypeSyns ->
                                          (NontermIdent -> Int -> [String] -> Code.Type) ->
                                          Bool ->
                                          (Set NontermIdent) ->
                                          ( Bool,Chunks,(Seq Error),(Set NontermIdent),(Map (NontermIdent, Int) ([String], Code.Type))))
data Inh_CNonterminal  = Inh_CNonterminal {allNts_Inh_CNonterminal :: !(Set NontermIdent),allPragmas_Inh_CNonterminal :: !(PragmaMap),contextMap_Inh_CNonterminal :: !(ContextMap),derivings_Inh_CNonterminal :: !(Derivings),errors_Inh_CNonterminal :: !(Seq Error),o_case_Inh_CNonterminal :: !(Bool),o_cata_Inh_CNonterminal :: !(Bool),o_costcentre_Inh_CNonterminal :: !(Bool),o_data_Inh_CNonterminal :: !(Maybe Bool),o_linePragmas_Inh_CNonterminal :: !(Bool),o_newtypes_Inh_CNonterminal :: !(Bool),o_pretty_Inh_CNonterminal :: !(Bool),o_rename_Inh_CNonterminal :: !(Bool),o_sem_Inh_CNonterminal :: !(Bool),o_sig_Inh_CNonterminal :: !(Bool),o_splitsems_Inh_CNonterminal :: !(Bool),o_strictwrap_Inh_CNonterminal :: !(Bool),o_traces_Inh_CNonterminal :: !(Bool),o_unbox_Inh_CNonterminal :: !(Bool),paramMap_Inh_CNonterminal :: !(ParamMap),prefix_Inh_CNonterminal :: !(String),typeSyns_Inh_CNonterminal :: !(TypeSyns),unfoldSemDom_Inh_CNonterminal :: !(NontermIdent -> Int -> [String] -> Code.Type),with_sig_Inh_CNonterminal :: !(Bool),wrappers_Inh_CNonterminal :: !(Set NontermIdent)}
data Syn_CNonterminal  = Syn_CNonterminal {allTpsFound_Syn_CNonterminal :: !(Bool),chunks_Syn_CNonterminal :: !(Chunks),errors_Syn_CNonterminal :: !(Seq Error),gathNts_Syn_CNonterminal :: !(Set NontermIdent),semDomUnfoldGath_Syn_CNonterminal :: !(Map (NontermIdent, Int) ([String], Code.Type))}
wrap_CNonterminal :: T_CNonterminal  ->
                     Inh_CNonterminal  ->
                     Syn_CNonterminal 
wrap_CNonterminal (T_CNonterminal sem ) (Inh_CNonterminal _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIderivings _lhsIerrors _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers )  =
    (let ( _lhsOallTpsFound,_lhsOchunks,_lhsOerrors,_lhsOgathNts,_lhsOsemDomUnfoldGath) =
             (sem _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIderivings _lhsIerrors _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers )
     in  (Syn_CNonterminal _lhsOallTpsFound _lhsOchunks _lhsOerrors _lhsOgathNts _lhsOsemDomUnfoldGath ))
sem_CNonterminal_CNonterminal :: NontermIdent ->
                                 ([Identifier]) ->
                                 Attributes ->
                                 Attributes ->
                                 T_CProductions  ->
                                 T_CInterface  ->
                                 T_CNonterminal 
sem_CNonterminal_CNonterminal nt_ params_ inh_ syn_ (T_CProductions prods_ ) (T_CInterface inter_ )  =
    (T_CNonterminal (\ _lhsIallNts
                       _lhsIallPragmas
                       _lhsIcontextMap
                       _lhsIderivings
                       _lhsIerrors
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_costcentre
                       _lhsIo_data
                       _lhsIo_linePragmas
                       _lhsIo_newtypes
                       _lhsIo_pretty
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_splitsems
                       _lhsIo_strictwrap
                       _lhsIo_traces
                       _lhsIo_unbox
                       _lhsIparamMap
                       _lhsIprefix
                       _lhsItypeSyns
                       _lhsIunfoldSemDom
                       _lhsIwith_sig
                       _lhsIwrappers ->
                         (let __tup1 :: ((Attributes,Attributes,NontermIdent))
                              _interOinh :: Attributes
                              _interOsyn :: Attributes
                              _interOnt :: NontermIdent
                              __tup2 :: ((Attributes,Attributes,NontermIdent))
                              _prodsOinh :: Attributes
                              _prodsOsyn :: Attributes
                              _prodsOnt :: NontermIdent
                              _lhsOgathNts :: (Set NontermIdent)
                              _lhsOchunks :: Chunks
                              _lhsOallTpsFound :: Bool
                              _lhsOerrors :: (Seq Error)
                              _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                              _prodsOallNts :: (Set NontermIdent)
                              _prodsOallPragmas :: PragmaMap
                              _prodsOcontextMap :: ContextMap
                              _prodsOerrors :: (Seq Error)
                              _prodsOo_case :: Bool
                              _prodsOo_cata :: Bool
                              _prodsOo_costcentre :: Bool
                              _prodsOo_data :: (Maybe Bool)
                              _prodsOo_linePragmas :: Bool
                              _prodsOo_newtypes :: Bool
                              _prodsOo_pretty :: Bool
                              _prodsOo_rename :: Bool
                              _prodsOo_sem :: Bool
                              _prodsOo_sig :: Bool
                              _prodsOo_splitsems :: Bool
                              _prodsOo_strictwrap :: Bool
                              _prodsOo_traces :: Bool
                              _prodsOo_unbox :: Bool
                              _prodsOparamMap :: ParamMap
                              _prodsOprefix :: String
                              _prodsOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                              _prodsOwith_sig :: Bool
                              _interOo_case :: Bool
                              _interOo_cata :: Bool
                              _interOo_costcentre :: Bool
                              _interOo_data :: (Maybe Bool)
                              _interOo_linePragmas :: Bool
                              _interOo_newtypes :: Bool
                              _interOo_pretty :: Bool
                              _interOo_rename :: Bool
                              _interOo_sem :: Bool
                              _interOo_sig :: Bool
                              _interOo_splitsems :: Bool
                              _interOo_strictwrap :: Bool
                              _interOo_traces :: Bool
                              _interOo_unbox :: Bool
                              _interOparamMap :: ParamMap
                              _interOprefix :: String
                              _prodsIallTpsFound :: Bool
                              _prodsIcataAlts :: Decls
                              _prodsIcomments :: ([String])
                              _prodsIdataAlts :: DataAlts
                              _prodsIdecls :: Decls
                              _prodsIerrors :: (Seq Error)
                              _prodsIsemNames :: ([String])
                              _interIcomments :: ([String])
                              _interIsemDom :: ([Decl])
                              _interIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                              _interIwrapDecls :: Decls
                              -- "GenerateCode.ag"(line 79, column 26)
                              __tup1 =
                                  (inh_,syn_,nt_)
                              -- "GenerateCode.ag"(line 79, column 26)
                              (_interOinh,_,_) =
                                  __tup1
                              -- "GenerateCode.ag"(line 79, column 26)
                              (_,_interOsyn,_) =
                                  __tup1
                              -- "GenerateCode.ag"(line 79, column 26)
                              (_,_,_interOnt) =
                                  __tup1
                              -- "GenerateCode.ag"(line 80, column 25)
                              __tup2 =
                                  (inh_,syn_,nt_)
                              -- "GenerateCode.ag"(line 80, column 25)
                              (_prodsOinh,_,_) =
                                  __tup2
                              -- "GenerateCode.ag"(line 80, column 25)
                              (_,_prodsOsyn,_) =
                                  __tup2
                              -- "GenerateCode.ag"(line 80, column 25)
                              (_,_,_prodsOnt) =
                                  __tup2
                              -- "GenerateCode.ag"(line 130, column 7)
                              _lhsOgathNts =
                                  Set.singleton nt_
                              -- "GenerateCode.ag"(line 659, column 18)
                              _semWrapper =
                                  let params' = map getName params_
                                      inhAttrs = Map.toList inh_
                                      synAttrs = Map.toList syn_
                                      inhVars = [ SimpleExpr (attrname True _LHS a) | (a,_) <- inhAttrs ]
                                      synVars = [ SimpleExpr (attrname False _LHS a) | (a,_) <- synAttrs ]
                                      var = "sem"
                                      wrapNT = "wrap" ++ "_" ++ getName nt_
                                      inhNT = "Inh" ++ "_" ++ getName nt_
                                      synNT = "Syn" ++ "_" ++ getName nt_
                                      varPat = if  _lhsIo_newtypes
                                                   then App (sdtype nt_) [SimpleExpr var]
                                                   else SimpleExpr var
                                      evalTp | null params' = id
                                             | otherwise    = idEvalType
                                      evalTpCommon t | null params' = t
                                                     | otherwise    = case (evalTp (SimpleType t)) of SimpleType t' -> t'
                                      appParams nm = TypeApp (SimpleType nm) (map SimpleType params')
                                      typeSig = TSig wrapNT (evalTp $ appParams (sdtype nt_) `Arr` (appParams inhNT `Arr` appParams synNT))
                                      mkstrict = if _lhsIo_strictwrap then (\x -> "!(" ++ x ++ ")") else id
                                      mkdata n attrs = Data n params' [Record n [(getName f++"_"++n,mkstrict $ evalTpCommon $ typeToHaskellString (Just nt_) params' t) | (f,t) <- attrs]] False []
                                      datas = [mkdata inhNT inhAttrs, mkdata synNT synAttrs]
                                  in datas ++ [ typeSig
                                              , Decl (Fun wrapNT [varPat, App inhNT inhVars])
                                                    (Let _interIwrapDecls (App synNT synVars))
                                                    Set.empty Set.empty
                                              ]
                              -- "GenerateCode.ag"(line 734, column 18)
                              _comment =
                                  Comment . unlines . map ind $ ( _interIcomments ++ ("alternatives:" : map ind _prodsIcomments) )
                              -- "GenerateCode.ag"(line 788, column 19)
                              _lhsOchunks =
                                  [ Chunk (getName nt_)
                                         (Comment (getName nt_ ++ " " ++ replicate (60 - length (getName nt_)) '-'))
                                         (if _lhsIo_pretty                  then [_comment    ]   else [])
                                         (if isJust _lhsIo_data             then [_dataDef    ]   else [])
                                         (if _lhsIo_cata                    then  _cataFun        else [])
                                         (if _lhsIo_sig                     then  _interIsemDom   else [])
                                         (if nt_ `Set.member` _lhsIwrappers then  _semWrapper     else [])
                                         (if _lhsIo_sem                     then  _prodsIdecls     else [])
                                         (if _lhsIo_sem                     then  _prodsIsemNames  else [])
                                  ]
                              -- "GenerateCode.ag"(line 843, column 18)
                              _dataDef =
                                  let params' = map getName params_
                                      typeSyn tp = let theType =
                                                         case tp of
                                                           CommonTypes.Maybe t      -> SimpleType ("Maybe (" ++ typeToHaskellString (Just nt_) params' t ++")")
                                                           CommonTypes.Either t1 t2 -> SimpleType ("Either (" ++ typeToHaskellString (Just nt_) params' t1 ++") ("
                                                                                                              ++ typeToHaskellString (Just nt_) params' t2 ++")")
                                                           CommonTypes.Map t1 t2    -> SimpleType ("Map (" ++ typeToHaskellString (Just nt_) params' t1 ++") ("
                                                                                                           ++ typeToHaskellString (Just nt_) params' t2 ++")")
                                                           CommonTypes.IntMap t     -> SimpleType ("IntMap (" ++ typeToHaskellString (Just nt_) params' t ++")")
                                                           CommonTypes.List t       -> Code.List $ SimpleType (typeToHaskellString (Just nt_) params' t)
                                                           CommonTypes.Tuple ts     -> Code.TupleType [SimpleType (typeToHaskellString (Just nt_) params' t)
                                                                                                  | (_,t) <- ts
                                                                                                  ]
                                                    in Code.Type (getName nt_) params' (idEvalType theType)
                                      derivings  = maybe [] (map getName . Set.toList) (Map.lookup nt_ _lhsIderivings)
                                      dataDef    = Data (getName nt_) (map getName params_) _prodsIdataAlts (maybe False id _lhsIo_data) derivings
                                  in maybe dataDef typeSyn $ lookup nt_ _lhsItypeSyns
                              -- "GenerateCode.ag"(line 877, column 18)
                              _cataFun =
                                  let appParams nm = TypeApp (SimpleType nm) (map SimpleType (map getName params_))
                                      evalTp | null params_ = id
                                             | otherwise    = idEvalType
                                      tSig = TSig (cataname _lhsIprefix nt_)
                                                  (appContext _lhsIcontextMap nt_ $ evalTp $ appParams (getName nt_) `Arr` appParams (sdtype nt_))
                                      special typ = case typ of
                                                    CommonTypes.List tp ->
                                                        let cons = SimpleExpr (semname _lhsIprefix nt_ (identifier "Cons"))
                                                            nil  = SimpleExpr (semname _lhsIprefix nt_ (identifier "Nil" ))
                                                            arg  = SimpleExpr "list"
                                                            rarg = case tp of
                                                                     NT t _ -> SimpleExpr ("(Prelude.map " ++ (cataname _lhsIprefix t) ++ " list)")
                                                                     _      -> arg
                                                            lhs = Fun (cataname _lhsIprefix nt_) [arg]
                                                            rhs = (App "Prelude.foldr" [cons,nil,rarg])
                                                        in  [Decl lhs rhs Set.empty Set.empty]
                                                    CommonTypes.Maybe tp ->
                                                        let just    = semname _lhsIprefix nt_ (identifier "Just")
                                                            nothing = semname _lhsIprefix nt_ (identifier "Nothing" )
                                                            arg  = SimpleExpr "x"
                                                            rarg = case tp of
                                                                     NT t _ -> App (cataname _lhsIprefix t) [arg]
                                                                     _      -> arg
                                                            lhs a = Fun (cataname _lhsIprefix nt_) [a]
                                                        in  [Decl (lhs (App "Prelude.Just" [arg]))     (App just [rarg])    Set.empty Set.empty
                                                            ,Decl (lhs (SimpleExpr "Prelude.Nothing")) (SimpleExpr nothing) Set.empty Set.empty
                                                            ]
                                                    CommonTypes.Either tp1 tp2 ->
                                                        let left  = semname _lhsIprefix nt_ (identifier "Left")
                                                            right = semname _lhsIprefix nt_ (identifier "Right" )
                                                            arg   = SimpleExpr "x"
                                                            rarg0 = case tp1 of
                                                                     NT t _ -> App (cataname _lhsIprefix t) [arg]
                                                                     _      -> arg
                                                            rarg1 = case tp2 of
                                                                     NT t _ -> App (cataname _lhsIprefix t) [arg]
                                                                     _      -> arg
                                                            lhs a = Fun (cataname _lhsIprefix nt_) [a]
                                                        in  [Decl (lhs (App "Prelude.Left"  [arg]))     (App left  [rarg0])    Set.empty Set.empty
                                                            ,Decl (lhs (App "Prelude.Right" [arg]))     (App right [rarg1])    Set.empty Set.empty
                                                            ]
                                                    CommonTypes.Map _ tp ->
                                                      let entry = SimpleExpr (semname _lhsIprefix nt_ (identifier "Entry"))
                                                          nil   = SimpleExpr (semname _lhsIprefix nt_ (identifier "Nil"))
                                                          arg   = SimpleExpr "m"
                                                          rarg  = case tp of
                                                                    NT t _ -> App "Map.map" [SimpleExpr $ cataname _lhsIprefix t, arg]
                                                                    _      -> arg
                                                          lhs   = Fun (cataname _lhsIprefix nt_) [arg]
                                                          rhs   = App "Map.foldWithKey" [entry,nil,rarg]
                                                      in [Decl lhs rhs Set.empty Set.empty]
                                                    CommonTypes.IntMap tp ->
                                                      let entry = SimpleExpr (semname _lhsIprefix nt_ (identifier "Entry"))
                                                          nil   = SimpleExpr (semname _lhsIprefix nt_ (identifier "Nil"))
                                                          arg   = SimpleExpr "m"
                                                          rarg  = case tp of
                                                                    NT t _ -> App "IntMap.map" [SimpleExpr $ cataname _lhsIprefix t, arg]
                                                                    _      -> arg
                                                          lhs   = Fun (cataname _lhsIprefix nt_) [arg]
                                                          rhs   = App "IntMap.foldWithKey" [entry,nil,rarg]
                                                      in [Decl lhs rhs Set.empty Set.empty]
                                                    CommonTypes.Tuple tps ->
                                                        let con  = semname _lhsIprefix nt_ (identifier "Tuple")
                                                            tps' = [ (SimpleExpr (getName x),y) | (x,y) <- tps]
                                                            rargs = map rarg tps'
                                                            rarg (n, tp) = case tp of
                                                                     NT t _ -> App (cataname _lhsIprefix t) [n]
                                                                     _      -> n
                                                            lhs = Fun (cataname _lhsIprefix nt_) [TupleExpr (map fst tps')]
                                                            rhs = App con rargs
                                                        in  [Decl lhs rhs Set.empty Set.empty]
                                  in  Comment "cata" :
                                      (if _lhsIo_sig then [tSig] else []) ++
                                      maybe _prodsIcataAlts special (lookup nt_ _lhsItypeSyns)
                              -- use rule "GenerateCode.ag"(line 706, column 39)
                              _lhsOallTpsFound =
                                  _prodsIallTpsFound
                              -- use rule "GenerateCode.ag"(line 719, column 32)
                              _lhsOerrors =
                                  _prodsIerrors
                              -- use rule "GenerateCode.ag"(line 611, column 86)
                              _lhsOsemDomUnfoldGath =
                                  _interIsemDomUnfoldGath
                              -- copy rule (down)
                              _prodsOallNts =
                                  _lhsIallNts
                              -- copy rule (down)
                              _prodsOallPragmas =
                                  _lhsIallPragmas
                              -- copy rule (down)
                              _prodsOcontextMap =
                                  _lhsIcontextMap
                              -- copy rule (down)
                              _prodsOerrors =
                                  _lhsIerrors
                              -- copy rule (down)
                              _prodsOo_case =
                                  _lhsIo_case
                              -- copy rule (down)
                              _prodsOo_cata =
                                  _lhsIo_cata
                              -- copy rule (down)
                              _prodsOo_costcentre =
                                  _lhsIo_costcentre
                              -- copy rule (down)
                              _prodsOo_data =
                                  _lhsIo_data
                              -- copy rule (down)
                              _prodsOo_linePragmas =
                                  _lhsIo_linePragmas
                              -- copy rule (down)
                              _prodsOo_newtypes =
                                  _lhsIo_newtypes
                              -- copy rule (down)
                              _prodsOo_pretty =
                                  _lhsIo_pretty
                              -- copy rule (down)
                              _prodsOo_rename =
                                  _lhsIo_rename
                              -- copy rule (down)
                              _prodsOo_sem =
                                  _lhsIo_sem
                              -- copy rule (down)
                              _prodsOo_sig =
                                  _lhsIo_sig
                              -- copy rule (down)
                              _prodsOo_splitsems =
                                  _lhsIo_splitsems
                              -- copy rule (down)
                              _prodsOo_strictwrap =
                                  _lhsIo_strictwrap
                              -- copy rule (down)
                              _prodsOo_traces =
                                  _lhsIo_traces
                              -- copy rule (down)
                              _prodsOo_unbox =
                                  _lhsIo_unbox
                              -- copy rule (down)
                              _prodsOparamMap =
                                  _lhsIparamMap
                              -- copy rule (down)
                              _prodsOprefix =
                                  _lhsIprefix
                              -- copy rule (down)
                              _prodsOunfoldSemDom =
                                  _lhsIunfoldSemDom
                              -- copy rule (down)
                              _prodsOwith_sig =
                                  _lhsIwith_sig
                              -- copy rule (down)
                              _interOo_case =
                                  _lhsIo_case
                              -- copy rule (down)
                              _interOo_cata =
                                  _lhsIo_cata
                              -- copy rule (down)
                              _interOo_costcentre =
                                  _lhsIo_costcentre
                              -- copy rule (down)
                              _interOo_data =
                                  _lhsIo_data
                              -- copy rule (down)
                              _interOo_linePragmas =
                                  _lhsIo_linePragmas
                              -- copy rule (down)
                              _interOo_newtypes =
                                  _lhsIo_newtypes
                              -- copy rule (down)
                              _interOo_pretty =
                                  _lhsIo_pretty
                              -- copy rule (down)
                              _interOo_rename =
                                  _lhsIo_rename
                              -- copy rule (down)
                              _interOo_sem =
                                  _lhsIo_sem
                              -- copy rule (down)
                              _interOo_sig =
                                  _lhsIo_sig
                              -- copy rule (down)
                              _interOo_splitsems =
                                  _lhsIo_splitsems
                              -- copy rule (down)
                              _interOo_strictwrap =
                                  _lhsIo_strictwrap
                              -- copy rule (down)
                              _interOo_traces =
                                  _lhsIo_traces
                              -- copy rule (down)
                              _interOo_unbox =
                                  _lhsIo_unbox
                              -- copy rule (down)
                              _interOparamMap =
                                  _lhsIparamMap
                              -- copy rule (down)
                              _interOprefix =
                                  _lhsIprefix
                              ( _prodsIallTpsFound,_prodsIcataAlts,_prodsIcomments,_prodsIdataAlts,_prodsIdecls,_prodsIerrors,_prodsIsemNames) =
                                  (prods_ _prodsOallNts _prodsOallPragmas _prodsOcontextMap _prodsOerrors _prodsOinh _prodsOnt _prodsOo_case _prodsOo_cata _prodsOo_costcentre _prodsOo_data _prodsOo_linePragmas _prodsOo_newtypes _prodsOo_pretty _prodsOo_rename _prodsOo_sem _prodsOo_sig _prodsOo_splitsems _prodsOo_strictwrap _prodsOo_traces _prodsOo_unbox _prodsOparamMap _prodsOprefix _prodsOsyn _prodsOunfoldSemDom _prodsOwith_sig )
                              ( _interIcomments,_interIsemDom,_interIsemDomUnfoldGath,_interIwrapDecls) =
                                  (inter_ _interOinh _interOnt _interOo_case _interOo_cata _interOo_costcentre _interOo_data _interOo_linePragmas _interOo_newtypes _interOo_pretty _interOo_rename _interOo_sem _interOo_sig _interOo_splitsems _interOo_strictwrap _interOo_traces _interOo_unbox _interOparamMap _interOprefix _interOsyn )
                          in  ( _lhsOallTpsFound,_lhsOchunks,_lhsOerrors,_lhsOgathNts,_lhsOsemDomUnfoldGath))) )
-- CNonterminals -----------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         contextMap           : ContextMap
         derivings            : Derivings
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramMap             : ParamMap
         prefix               : String
         typeSyns             : TypeSyns
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
         wrappers             : Set NontermIdent
      chained attribute:
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         chunks               : Chunks
         gathNts              : Set NontermIdent
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
   alternatives:
      alternative Cons:
         child hd             : CNonterminal 
         child tl             : CNonterminals 
      alternative Nil:
-}
-- cata
sem_CNonterminals :: CNonterminals  ->
                     T_CNonterminals 
sem_CNonterminals list  =
    (Prelude.foldr sem_CNonterminals_Cons sem_CNonterminals_Nil (Prelude.map sem_CNonterminal list) )
-- semantic domain
newtype T_CNonterminals  = T_CNonterminals ((Set NontermIdent) ->
                                            PragmaMap ->
                                            ContextMap ->
                                            Derivings ->
                                            (Seq Error) ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            (Maybe Bool) ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            Bool ->
                                            ParamMap ->
                                            String ->
                                            TypeSyns ->
                                            (NontermIdent -> Int -> [String] -> Code.Type) ->
                                            Bool ->
                                            (Set NontermIdent) ->
                                            ( Bool,Chunks,(Seq Error),(Set NontermIdent),(Map (NontermIdent, Int) ([String], Code.Type))))
data Inh_CNonterminals  = Inh_CNonterminals {allNts_Inh_CNonterminals :: !(Set NontermIdent),allPragmas_Inh_CNonterminals :: !(PragmaMap),contextMap_Inh_CNonterminals :: !(ContextMap),derivings_Inh_CNonterminals :: !(Derivings),errors_Inh_CNonterminals :: !(Seq Error),o_case_Inh_CNonterminals :: !(Bool),o_cata_Inh_CNonterminals :: !(Bool),o_costcentre_Inh_CNonterminals :: !(Bool),o_data_Inh_CNonterminals :: !(Maybe Bool),o_linePragmas_Inh_CNonterminals :: !(Bool),o_newtypes_Inh_CNonterminals :: !(Bool),o_pretty_Inh_CNonterminals :: !(Bool),o_rename_Inh_CNonterminals :: !(Bool),o_sem_Inh_CNonterminals :: !(Bool),o_sig_Inh_CNonterminals :: !(Bool),o_splitsems_Inh_CNonterminals :: !(Bool),o_strictwrap_Inh_CNonterminals :: !(Bool),o_traces_Inh_CNonterminals :: !(Bool),o_unbox_Inh_CNonterminals :: !(Bool),paramMap_Inh_CNonterminals :: !(ParamMap),prefix_Inh_CNonterminals :: !(String),typeSyns_Inh_CNonterminals :: !(TypeSyns),unfoldSemDom_Inh_CNonterminals :: !(NontermIdent -> Int -> [String] -> Code.Type),with_sig_Inh_CNonterminals :: !(Bool),wrappers_Inh_CNonterminals :: !(Set NontermIdent)}
data Syn_CNonterminals  = Syn_CNonterminals {allTpsFound_Syn_CNonterminals :: !(Bool),chunks_Syn_CNonterminals :: !(Chunks),errors_Syn_CNonterminals :: !(Seq Error),gathNts_Syn_CNonterminals :: !(Set NontermIdent),semDomUnfoldGath_Syn_CNonterminals :: !(Map (NontermIdent, Int) ([String], Code.Type))}
wrap_CNonterminals :: T_CNonterminals  ->
                      Inh_CNonterminals  ->
                      Syn_CNonterminals 
wrap_CNonterminals (T_CNonterminals sem ) (Inh_CNonterminals _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIderivings _lhsIerrors _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers )  =
    (let ( _lhsOallTpsFound,_lhsOchunks,_lhsOerrors,_lhsOgathNts,_lhsOsemDomUnfoldGath) =
             (sem _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIderivings _lhsIerrors _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsItypeSyns _lhsIunfoldSemDom _lhsIwith_sig _lhsIwrappers )
     in  (Syn_CNonterminals _lhsOallTpsFound _lhsOchunks _lhsOerrors _lhsOgathNts _lhsOsemDomUnfoldGath ))
sem_CNonterminals_Cons :: T_CNonterminal  ->
                          T_CNonterminals  ->
                          T_CNonterminals 
sem_CNonterminals_Cons (T_CNonterminal hd_ ) (T_CNonterminals tl_ )  =
    (T_CNonterminals (\ _lhsIallNts
                        _lhsIallPragmas
                        _lhsIcontextMap
                        _lhsIderivings
                        _lhsIerrors
                        _lhsIo_case
                        _lhsIo_cata
                        _lhsIo_costcentre
                        _lhsIo_data
                        _lhsIo_linePragmas
                        _lhsIo_newtypes
                        _lhsIo_pretty
                        _lhsIo_rename
                        _lhsIo_sem
                        _lhsIo_sig
                        _lhsIo_splitsems
                        _lhsIo_strictwrap
                        _lhsIo_traces
                        _lhsIo_unbox
                        _lhsIparamMap
                        _lhsIprefix
                        _lhsItypeSyns
                        _lhsIunfoldSemDom
                        _lhsIwith_sig
                        _lhsIwrappers ->
                          (let _lhsOallTpsFound :: Bool
                               _lhsOchunks :: Chunks
                               _lhsOerrors :: (Seq Error)
                               _lhsOgathNts :: (Set NontermIdent)
                               _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               _hdOallNts :: (Set NontermIdent)
                               _hdOallPragmas :: PragmaMap
                               _hdOcontextMap :: ContextMap
                               _hdOderivings :: Derivings
                               _hdOerrors :: (Seq Error)
                               _hdOo_case :: Bool
                               _hdOo_cata :: Bool
                               _hdOo_costcentre :: Bool
                               _hdOo_data :: (Maybe Bool)
                               _hdOo_linePragmas :: Bool
                               _hdOo_newtypes :: Bool
                               _hdOo_pretty :: Bool
                               _hdOo_rename :: Bool
                               _hdOo_sem :: Bool
                               _hdOo_sig :: Bool
                               _hdOo_splitsems :: Bool
                               _hdOo_strictwrap :: Bool
                               _hdOo_traces :: Bool
                               _hdOo_unbox :: Bool
                               _hdOparamMap :: ParamMap
                               _hdOprefix :: String
                               _hdOtypeSyns :: TypeSyns
                               _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                               _hdOwith_sig :: Bool
                               _hdOwrappers :: (Set NontermIdent)
                               _tlOallNts :: (Set NontermIdent)
                               _tlOallPragmas :: PragmaMap
                               _tlOcontextMap :: ContextMap
                               _tlOderivings :: Derivings
                               _tlOerrors :: (Seq Error)
                               _tlOo_case :: Bool
                               _tlOo_cata :: Bool
                               _tlOo_costcentre :: Bool
                               _tlOo_data :: (Maybe Bool)
                               _tlOo_linePragmas :: Bool
                               _tlOo_newtypes :: Bool
                               _tlOo_pretty :: Bool
                               _tlOo_rename :: Bool
                               _tlOo_sem :: Bool
                               _tlOo_sig :: Bool
                               _tlOo_splitsems :: Bool
                               _tlOo_strictwrap :: Bool
                               _tlOo_traces :: Bool
                               _tlOo_unbox :: Bool
                               _tlOparamMap :: ParamMap
                               _tlOprefix :: String
                               _tlOtypeSyns :: TypeSyns
                               _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                               _tlOwith_sig :: Bool
                               _tlOwrappers :: (Set NontermIdent)
                               _hdIallTpsFound :: Bool
                               _hdIchunks :: Chunks
                               _hdIerrors :: (Seq Error)
                               _hdIgathNts :: (Set NontermIdent)
                               _hdIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               _tlIallTpsFound :: Bool
                               _tlIchunks :: Chunks
                               _tlIerrors :: (Seq Error)
                               _tlIgathNts :: (Set NontermIdent)
                               _tlIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               -- use rule "GenerateCode.ag"(line 706, column 39)
                               _lhsOallTpsFound =
                                   _hdIallTpsFound && _tlIallTpsFound
                               -- use rule "GenerateCode.ag"(line 780, column 49)
                               _lhsOchunks =
                                   _hdIchunks ++ _tlIchunks
                               -- use rule "GenerateCode.ag"(line 719, column 32)
                               _lhsOerrors =
                                   _hdIerrors Seq.>< _tlIerrors
                               -- use rule "GenerateCode.ag"(line 126, column 47)
                               _lhsOgathNts =
                                   _hdIgathNts `Set.union` _tlIgathNts
                               -- use rule "GenerateCode.ag"(line 611, column 86)
                               _lhsOsemDomUnfoldGath =
                                   _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
                               -- copy rule (down)
                               _hdOallNts =
                                   _lhsIallNts
                               -- copy rule (down)
                               _hdOallPragmas =
                                   _lhsIallPragmas
                               -- copy rule (down)
                               _hdOcontextMap =
                                   _lhsIcontextMap
                               -- copy rule (down)
                               _hdOderivings =
                                   _lhsIderivings
                               -- copy rule (down)
                               _hdOerrors =
                                   _lhsIerrors
                               -- copy rule (down)
                               _hdOo_case =
                                   _lhsIo_case
                               -- copy rule (down)
                               _hdOo_cata =
                                   _lhsIo_cata
                               -- copy rule (down)
                               _hdOo_costcentre =
                                   _lhsIo_costcentre
                               -- copy rule (down)
                               _hdOo_data =
                                   _lhsIo_data
                               -- copy rule (down)
                               _hdOo_linePragmas =
                                   _lhsIo_linePragmas
                               -- copy rule (down)
                               _hdOo_newtypes =
                                   _lhsIo_newtypes
                               -- copy rule (down)
                               _hdOo_pretty =
                                   _lhsIo_pretty
                               -- copy rule (down)
                               _hdOo_rename =
                                   _lhsIo_rename
                               -- copy rule (down)
                               _hdOo_sem =
                                   _lhsIo_sem
                               -- copy rule (down)
                               _hdOo_sig =
                                   _lhsIo_sig
                               -- copy rule (down)
                               _hdOo_splitsems =
                                   _lhsIo_splitsems
                               -- copy rule (down)
                               _hdOo_strictwrap =
                                   _lhsIo_strictwrap
                               -- copy rule (down)
                               _hdOo_traces =
                                   _lhsIo_traces
                               -- copy rule (down)
                               _hdOo_unbox =
                                   _lhsIo_unbox
                               -- copy rule (down)
                               _hdOparamMap =
                                   _lhsIparamMap
                               -- copy rule (down)
                               _hdOprefix =
                                   _lhsIprefix
                               -- copy rule (down)
                               _hdOtypeSyns =
                                   _lhsItypeSyns
                               -- copy rule (down)
                               _hdOunfoldSemDom =
                                   _lhsIunfoldSemDom
                               -- copy rule (down)
                               _hdOwith_sig =
                                   _lhsIwith_sig
                               -- copy rule (down)
                               _hdOwrappers =
                                   _lhsIwrappers
                               -- copy rule (down)
                               _tlOallNts =
                                   _lhsIallNts
                               -- copy rule (down)
                               _tlOallPragmas =
                                   _lhsIallPragmas
                               -- copy rule (down)
                               _tlOcontextMap =
                                   _lhsIcontextMap
                               -- copy rule (down)
                               _tlOderivings =
                                   _lhsIderivings
                               -- copy rule (chain)
                               _tlOerrors =
                                   _hdIerrors
                               -- copy rule (down)
                               _tlOo_case =
                                   _lhsIo_case
                               -- copy rule (down)
                               _tlOo_cata =
                                   _lhsIo_cata
                               -- copy rule (down)
                               _tlOo_costcentre =
                                   _lhsIo_costcentre
                               -- copy rule (down)
                               _tlOo_data =
                                   _lhsIo_data
                               -- copy rule (down)
                               _tlOo_linePragmas =
                                   _lhsIo_linePragmas
                               -- copy rule (down)
                               _tlOo_newtypes =
                                   _lhsIo_newtypes
                               -- copy rule (down)
                               _tlOo_pretty =
                                   _lhsIo_pretty
                               -- copy rule (down)
                               _tlOo_rename =
                                   _lhsIo_rename
                               -- copy rule (down)
                               _tlOo_sem =
                                   _lhsIo_sem
                               -- copy rule (down)
                               _tlOo_sig =
                                   _lhsIo_sig
                               -- copy rule (down)
                               _tlOo_splitsems =
                                   _lhsIo_splitsems
                               -- copy rule (down)
                               _tlOo_strictwrap =
                                   _lhsIo_strictwrap
                               -- copy rule (down)
                               _tlOo_traces =
                                   _lhsIo_traces
                               -- copy rule (down)
                               _tlOo_unbox =
                                   _lhsIo_unbox
                               -- copy rule (down)
                               _tlOparamMap =
                                   _lhsIparamMap
                               -- copy rule (down)
                               _tlOprefix =
                                   _lhsIprefix
                               -- copy rule (down)
                               _tlOtypeSyns =
                                   _lhsItypeSyns
                               -- copy rule (down)
                               _tlOunfoldSemDom =
                                   _lhsIunfoldSemDom
                               -- copy rule (down)
                               _tlOwith_sig =
                                   _lhsIwith_sig
                               -- copy rule (down)
                               _tlOwrappers =
                                   _lhsIwrappers
                               ( _hdIallTpsFound,_hdIchunks,_hdIerrors,_hdIgathNts,_hdIsemDomUnfoldGath) =
                                   (hd_ _hdOallNts _hdOallPragmas _hdOcontextMap _hdOderivings _hdOerrors _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOparamMap _hdOprefix _hdOtypeSyns _hdOunfoldSemDom _hdOwith_sig _hdOwrappers )
                               ( _tlIallTpsFound,_tlIchunks,_tlIerrors,_tlIgathNts,_tlIsemDomUnfoldGath) =
                                   (tl_ _tlOallNts _tlOallPragmas _tlOcontextMap _tlOderivings _tlOerrors _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOparamMap _tlOprefix _tlOtypeSyns _tlOunfoldSemDom _tlOwith_sig _tlOwrappers )
                           in  ( _lhsOallTpsFound,_lhsOchunks,_lhsOerrors,_lhsOgathNts,_lhsOsemDomUnfoldGath))) )
sem_CNonterminals_Nil :: T_CNonterminals 
sem_CNonterminals_Nil  =
    (T_CNonterminals (\ _lhsIallNts
                        _lhsIallPragmas
                        _lhsIcontextMap
                        _lhsIderivings
                        _lhsIerrors
                        _lhsIo_case
                        _lhsIo_cata
                        _lhsIo_costcentre
                        _lhsIo_data
                        _lhsIo_linePragmas
                        _lhsIo_newtypes
                        _lhsIo_pretty
                        _lhsIo_rename
                        _lhsIo_sem
                        _lhsIo_sig
                        _lhsIo_splitsems
                        _lhsIo_strictwrap
                        _lhsIo_traces
                        _lhsIo_unbox
                        _lhsIparamMap
                        _lhsIprefix
                        _lhsItypeSyns
                        _lhsIunfoldSemDom
                        _lhsIwith_sig
                        _lhsIwrappers ->
                          (let _lhsOallTpsFound :: Bool
                               _lhsOchunks :: Chunks
                               _lhsOerrors :: (Seq Error)
                               _lhsOgathNts :: (Set NontermIdent)
                               _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                               -- use rule "GenerateCode.ag"(line 706, column 39)
                               _lhsOallTpsFound =
                                   True
                               -- use rule "GenerateCode.ag"(line 780, column 49)
                               _lhsOchunks =
                                   []
                               -- use rule "GenerateCode.ag"(line 719, column 32)
                               _lhsOerrors =
                                   Seq.empty
                               -- use rule "GenerateCode.ag"(line 126, column 47)
                               _lhsOgathNts =
                                   Set.empty
                               -- use rule "GenerateCode.ag"(line 611, column 86)
                               _lhsOsemDomUnfoldGath =
                                   Map.empty
                           in  ( _lhsOallTpsFound,_lhsOchunks,_lhsOerrors,_lhsOgathNts,_lhsOsemDomUnfoldGath))) )
-- CProduction -------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         contextMap           : ContextMap
         inh                  : Attributes
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
      chained attribute:
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         cataAlt              : Decl
         comments             : [String]
         dataAlt              : DataAlt
         decls                : Decls
         semNames             : [String]
   alternatives:
      alternative CProduction:
         child con            : {ConstructorIdent}
         child visits         : CVisits 
         child children       : {[(Identifier,Type,Bool)]}
         child terminals      : {[Identifier]}
         visit 0:
            local paramInstMap : _
            local firstOrderChildren : _
            local params      : _
-}
-- cata
sem_CProduction :: CProduction  ->
                   T_CProduction 
sem_CProduction (CProduction _con _visits _children _terminals )  =
    (sem_CProduction_CProduction _con (sem_CVisits _visits ) _children _terminals )
-- semantic domain
newtype T_CProduction  = T_CProduction ((Set NontermIdent) ->
                                        PragmaMap ->
                                        ContextMap ->
                                        (Seq Error) ->
                                        Attributes ->
                                        NontermIdent ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        (Maybe Bool) ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        Bool ->
                                        ParamMap ->
                                        String ->
                                        Attributes ->
                                        (NontermIdent -> Int -> [String] -> Code.Type) ->
                                        Bool ->
                                        ( Bool,Decl,([String]),DataAlt,Decls,(Seq Error),([String])))
data Inh_CProduction  = Inh_CProduction {allNts_Inh_CProduction :: !(Set NontermIdent),allPragmas_Inh_CProduction :: !(PragmaMap),contextMap_Inh_CProduction :: !(ContextMap),errors_Inh_CProduction :: !(Seq Error),inh_Inh_CProduction :: !(Attributes),nt_Inh_CProduction :: !(NontermIdent),o_case_Inh_CProduction :: !(Bool),o_cata_Inh_CProduction :: !(Bool),o_costcentre_Inh_CProduction :: !(Bool),o_data_Inh_CProduction :: !(Maybe Bool),o_linePragmas_Inh_CProduction :: !(Bool),o_newtypes_Inh_CProduction :: !(Bool),o_pretty_Inh_CProduction :: !(Bool),o_rename_Inh_CProduction :: !(Bool),o_sem_Inh_CProduction :: !(Bool),o_sig_Inh_CProduction :: !(Bool),o_splitsems_Inh_CProduction :: !(Bool),o_strictwrap_Inh_CProduction :: !(Bool),o_traces_Inh_CProduction :: !(Bool),o_unbox_Inh_CProduction :: !(Bool),paramMap_Inh_CProduction :: !(ParamMap),prefix_Inh_CProduction :: !(String),syn_Inh_CProduction :: !(Attributes),unfoldSemDom_Inh_CProduction :: !(NontermIdent -> Int -> [String] -> Code.Type),with_sig_Inh_CProduction :: !(Bool)}
data Syn_CProduction  = Syn_CProduction {allTpsFound_Syn_CProduction :: !(Bool),cataAlt_Syn_CProduction :: !(Decl),comments_Syn_CProduction :: !([String]),dataAlt_Syn_CProduction :: !(DataAlt),decls_Syn_CProduction :: !(Decls),errors_Syn_CProduction :: !(Seq Error),semNames_Syn_CProduction :: !([String])}
wrap_CProduction :: T_CProduction  ->
                    Inh_CProduction  ->
                    Syn_CProduction 
wrap_CProduction (T_CProduction sem ) (Inh_CProduction _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIerrors _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig )  =
    (let ( _lhsOallTpsFound,_lhsOcataAlt,_lhsOcomments,_lhsOdataAlt,_lhsOdecls,_lhsOerrors,_lhsOsemNames) =
             (sem _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIerrors _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig )
     in  (Syn_CProduction _lhsOallTpsFound _lhsOcataAlt _lhsOcomments _lhsOdataAlt _lhsOdecls _lhsOerrors _lhsOsemNames ))
sem_CProduction_CProduction :: ConstructorIdent ->
                               T_CVisits  ->
                               ([(Identifier,Type,Bool)]) ->
                               ([Identifier]) ->
                               T_CProduction 
sem_CProduction_CProduction con_ (T_CVisits visits_ ) children_ terminals_  =
    (T_CProduction (\ _lhsIallNts
                      _lhsIallPragmas
                      _lhsIcontextMap
                      _lhsIerrors
                      _lhsIinh
                      _lhsInt
                      _lhsIo_case
                      _lhsIo_cata
                      _lhsIo_costcentre
                      _lhsIo_data
                      _lhsIo_linePragmas
                      _lhsIo_newtypes
                      _lhsIo_pretty
                      _lhsIo_rename
                      _lhsIo_sem
                      _lhsIo_sig
                      _lhsIo_splitsems
                      _lhsIo_strictwrap
                      _lhsIo_traces
                      _lhsIo_unbox
                      _lhsIparamMap
                      _lhsIprefix
                      _lhsIsyn
                      _lhsIunfoldSemDom
                      _lhsIwith_sig ->
                        (let _visitsOcon :: ConstructorIdent
                             _visitsOterminals :: ([Identifier])
                             _visitsOnr :: Int
                             _visitsOchildren :: ([(Identifier,Type,Bool)])
                             _visitsOinstVisitNrs :: (Map Identifier Int)
                             _lhsOcomments :: ([String])
                             _lhsOdataAlt :: DataAlt
                             _lhsOcataAlt :: Decl
                             _lhsOallTpsFound :: Bool
                             _lhsOdecls :: Decls
                             _lhsOerrors :: (Seq Error)
                             _lhsOsemNames :: ([String])
                             _visitsOallNts :: (Set NontermIdent)
                             _visitsOallPragmas :: PragmaMap
                             _visitsOcontextMap :: ContextMap
                             _visitsOerrors :: (Seq Error)
                             _visitsOinh :: Attributes
                             _visitsOnt :: NontermIdent
                             _visitsOo_case :: Bool
                             _visitsOo_cata :: Bool
                             _visitsOo_costcentre :: Bool
                             _visitsOo_data :: (Maybe Bool)
                             _visitsOo_linePragmas :: Bool
                             _visitsOo_newtypes :: Bool
                             _visitsOo_pretty :: Bool
                             _visitsOo_rename :: Bool
                             _visitsOo_sem :: Bool
                             _visitsOo_sig :: Bool
                             _visitsOo_splitsems :: Bool
                             _visitsOo_strictwrap :: Bool
                             _visitsOo_traces :: Bool
                             _visitsOo_unbox :: Bool
                             _visitsOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                             _visitsOparamMap :: ParamMap
                             _visitsOprefix :: String
                             _visitsOsyn :: Attributes
                             _visitsOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                             _visitsOwith_sig :: Bool
                             _visitsIallTpsFound :: Bool
                             _visitsIcomments :: ([String])
                             _visitsIdecls :: Decls
                             _visitsIerrors :: (Seq Error)
                             _visitsIgatherInstVisitNrs :: (Map Identifier Int)
                             _visitsIintra :: Exprs
                             _visitsIintraVars :: (Set String)
                             _visitsIisNil :: Bool
                             _visitsIsemNames :: ([String])
                             -- "GenerateCode.ag"(line 85, column 19)
                             _visitsOcon =
                                 con_
                             -- "GenerateCode.ag"(line 86, column 20)
                             _visitsOterminals =
                                 terminals_
                             -- "GenerateCode.ag"(line 98, column 7)
                             _paramInstMap =
                                 Map.fromList [(nm, (extractNonterminal tp, tps)) | (nm,tp,_) <- children_, let tps = map cleanupArg $ nontermArgs tp, not (null tps) ]
                             -- "GenerateCode.ag"(line 214, column 18)
                             _visitsOnr =
                                 0
                             -- "GenerateCode.ag"(line 340, column 18)
                             _visitsOchildren =
                                 children_
                             -- "GenerateCode.ag"(line 450, column 7)
                             _visitsOinstVisitNrs =
                                 _visitsIgatherInstVisitNrs
                             -- "GenerateCode.ag"(line 749, column 17)
                             _firstOrderChildren =
                                 filter (\(_,_,ho) -> not ho) children_
                             -- "GenerateCode.ag"(line 750, column 18)
                             _lhsOcomments =
                                 ("alternative " ++ getName con_ ++ ":")
                                 : map ind (  map (\(x,y,_) -> makeLocalComment 14 "child" x (Just y)) _firstOrderChildren
                                           ++ _visitsIcomments
                                           )
                             -- "GenerateCode.ag"(line 869, column 17)
                             _params =
                                 map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                             -- "GenerateCode.ag"(line 870, column 17)
                             _lhsOdataAlt =
                                 DataAlt (conname _lhsIo_rename _lhsInt con_) (map (\(_,t,_) -> typeToHaskellString (Just _lhsInt) _params     t) _firstOrderChildren    )
                             -- "GenerateCode.ag"(line 962, column 17)
                             _lhsOcataAlt =
                                 let lhs = Fun (cataname _lhsIprefix _lhsInt) [lhs_pat]
                                     lhs_pat = App (conname _lhsIo_rename _lhsInt con_)
                                                    (map (\(n,_,_) -> SimpleExpr $ locname $ n) _firstOrderChildren    )
                                     rhs = App (semname _lhsIprefix _lhsInt con_)
                                                (map argument _firstOrderChildren    )
                                     argument (nm,NT tp _,_) = App (cataname _lhsIprefix tp)
                                                                  [SimpleExpr (locname nm)]
                                     argument (nm, _,_)    = SimpleExpr (locname nm)
                                  in Decl lhs rhs Set.empty Set.empty
                             -- use rule "GenerateCode.ag"(line 706, column 39)
                             _lhsOallTpsFound =
                                 _visitsIallTpsFound
                             -- use rule "GenerateCode.ag"(line 781, column 44)
                             _lhsOdecls =
                                 _visitsIdecls
                             -- use rule "GenerateCode.ag"(line 719, column 32)
                             _lhsOerrors =
                                 _visitsIerrors
                             -- use rule "GenerateCode.ag"(line 977, column 61)
                             _lhsOsemNames =
                                 _visitsIsemNames
                             -- copy rule (down)
                             _visitsOallNts =
                                 _lhsIallNts
                             -- copy rule (down)
                             _visitsOallPragmas =
                                 _lhsIallPragmas
                             -- copy rule (down)
                             _visitsOcontextMap =
                                 _lhsIcontextMap
                             -- copy rule (down)
                             _visitsOerrors =
                                 _lhsIerrors
                             -- copy rule (down)
                             _visitsOinh =
                                 _lhsIinh
                             -- copy rule (down)
                             _visitsOnt =
                                 _lhsInt
                             -- copy rule (down)
                             _visitsOo_case =
                                 _lhsIo_case
                             -- copy rule (down)
                             _visitsOo_cata =
                                 _lhsIo_cata
                             -- copy rule (down)
                             _visitsOo_costcentre =
                                 _lhsIo_costcentre
                             -- copy rule (down)
                             _visitsOo_data =
                                 _lhsIo_data
                             -- copy rule (down)
                             _visitsOo_linePragmas =
                                 _lhsIo_linePragmas
                             -- copy rule (down)
                             _visitsOo_newtypes =
                                 _lhsIo_newtypes
                             -- copy rule (down)
                             _visitsOo_pretty =
                                 _lhsIo_pretty
                             -- copy rule (down)
                             _visitsOo_rename =
                                 _lhsIo_rename
                             -- copy rule (down)
                             _visitsOo_sem =
                                 _lhsIo_sem
                             -- copy rule (down)
                             _visitsOo_sig =
                                 _lhsIo_sig
                             -- copy rule (down)
                             _visitsOo_splitsems =
                                 _lhsIo_splitsems
                             -- copy rule (down)
                             _visitsOo_strictwrap =
                                 _lhsIo_strictwrap
                             -- copy rule (down)
                             _visitsOo_traces =
                                 _lhsIo_traces
                             -- copy rule (down)
                             _visitsOo_unbox =
                                 _lhsIo_unbox
                             -- copy rule (from local)
                             _visitsOparamInstMap =
                                 _paramInstMap
                             -- copy rule (down)
                             _visitsOparamMap =
                                 _lhsIparamMap
                             -- copy rule (down)
                             _visitsOprefix =
                                 _lhsIprefix
                             -- copy rule (down)
                             _visitsOsyn =
                                 _lhsIsyn
                             -- copy rule (down)
                             _visitsOunfoldSemDom =
                                 _lhsIunfoldSemDom
                             -- copy rule (down)
                             _visitsOwith_sig =
                                 _lhsIwith_sig
                             ( _visitsIallTpsFound,_visitsIcomments,_visitsIdecls,_visitsIerrors,_visitsIgatherInstVisitNrs,_visitsIintra,_visitsIintraVars,_visitsIisNil,_visitsIsemNames) =
                                 (visits_ _visitsOallNts _visitsOallPragmas _visitsOchildren _visitsOcon _visitsOcontextMap _visitsOerrors _visitsOinh _visitsOinstVisitNrs _visitsOnr _visitsOnt _visitsOo_case _visitsOo_cata _visitsOo_costcentre _visitsOo_data _visitsOo_linePragmas _visitsOo_newtypes _visitsOo_pretty _visitsOo_rename _visitsOo_sem _visitsOo_sig _visitsOo_splitsems _visitsOo_strictwrap _visitsOo_traces _visitsOo_unbox _visitsOparamInstMap _visitsOparamMap _visitsOprefix _visitsOsyn _visitsOterminals _visitsOunfoldSemDom _visitsOwith_sig )
                         in  ( _lhsOallTpsFound,_lhsOcataAlt,_lhsOcomments,_lhsOdataAlt,_lhsOdecls,_lhsOerrors,_lhsOsemNames))) )
-- CProductions ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         contextMap           : ContextMap
         inh                  : Attributes
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
      chained attribute:
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         cataAlts             : Decls
         comments             : [String]
         dataAlts             : DataAlts
         decls                : Decls
         semNames             : [String]
   alternatives:
      alternative Cons:
         child hd             : CProduction 
         child tl             : CProductions 
      alternative Nil:
-}
-- cata
sem_CProductions :: CProductions  ->
                    T_CProductions 
sem_CProductions list  =
    (Prelude.foldr sem_CProductions_Cons sem_CProductions_Nil (Prelude.map sem_CProduction list) )
-- semantic domain
newtype T_CProductions  = T_CProductions ((Set NontermIdent) ->
                                          PragmaMap ->
                                          ContextMap ->
                                          (Seq Error) ->
                                          Attributes ->
                                          NontermIdent ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          (Maybe Bool) ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          Bool ->
                                          ParamMap ->
                                          String ->
                                          Attributes ->
                                          (NontermIdent -> Int -> [String] -> Code.Type) ->
                                          Bool ->
                                          ( Bool,Decls,([String]),DataAlts,Decls,(Seq Error),([String])))
data Inh_CProductions  = Inh_CProductions {allNts_Inh_CProductions :: !(Set NontermIdent),allPragmas_Inh_CProductions :: !(PragmaMap),contextMap_Inh_CProductions :: !(ContextMap),errors_Inh_CProductions :: !(Seq Error),inh_Inh_CProductions :: !(Attributes),nt_Inh_CProductions :: !(NontermIdent),o_case_Inh_CProductions :: !(Bool),o_cata_Inh_CProductions :: !(Bool),o_costcentre_Inh_CProductions :: !(Bool),o_data_Inh_CProductions :: !(Maybe Bool),o_linePragmas_Inh_CProductions :: !(Bool),o_newtypes_Inh_CProductions :: !(Bool),o_pretty_Inh_CProductions :: !(Bool),o_rename_Inh_CProductions :: !(Bool),o_sem_Inh_CProductions :: !(Bool),o_sig_Inh_CProductions :: !(Bool),o_splitsems_Inh_CProductions :: !(Bool),o_strictwrap_Inh_CProductions :: !(Bool),o_traces_Inh_CProductions :: !(Bool),o_unbox_Inh_CProductions :: !(Bool),paramMap_Inh_CProductions :: !(ParamMap),prefix_Inh_CProductions :: !(String),syn_Inh_CProductions :: !(Attributes),unfoldSemDom_Inh_CProductions :: !(NontermIdent -> Int -> [String] -> Code.Type),with_sig_Inh_CProductions :: !(Bool)}
data Syn_CProductions  = Syn_CProductions {allTpsFound_Syn_CProductions :: !(Bool),cataAlts_Syn_CProductions :: !(Decls),comments_Syn_CProductions :: !([String]),dataAlts_Syn_CProductions :: !(DataAlts),decls_Syn_CProductions :: !(Decls),errors_Syn_CProductions :: !(Seq Error),semNames_Syn_CProductions :: !([String])}
wrap_CProductions :: T_CProductions  ->
                     Inh_CProductions  ->
                     Syn_CProductions 
wrap_CProductions (T_CProductions sem ) (Inh_CProductions _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIerrors _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig )  =
    (let ( _lhsOallTpsFound,_lhsOcataAlts,_lhsOcomments,_lhsOdataAlts,_lhsOdecls,_lhsOerrors,_lhsOsemNames) =
             (sem _lhsIallNts _lhsIallPragmas _lhsIcontextMap _lhsIerrors _lhsIinh _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIunfoldSemDom _lhsIwith_sig )
     in  (Syn_CProductions _lhsOallTpsFound _lhsOcataAlts _lhsOcomments _lhsOdataAlts _lhsOdecls _lhsOerrors _lhsOsemNames ))
sem_CProductions_Cons :: T_CProduction  ->
                         T_CProductions  ->
                         T_CProductions 
sem_CProductions_Cons (T_CProduction hd_ ) (T_CProductions tl_ )  =
    (T_CProductions (\ _lhsIallNts
                       _lhsIallPragmas
                       _lhsIcontextMap
                       _lhsIerrors
                       _lhsIinh
                       _lhsInt
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_costcentre
                       _lhsIo_data
                       _lhsIo_linePragmas
                       _lhsIo_newtypes
                       _lhsIo_pretty
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_splitsems
                       _lhsIo_strictwrap
                       _lhsIo_traces
                       _lhsIo_unbox
                       _lhsIparamMap
                       _lhsIprefix
                       _lhsIsyn
                       _lhsIunfoldSemDom
                       _lhsIwith_sig ->
                         (let _lhsOdataAlts :: DataAlts
                              _lhsOcataAlts :: Decls
                              _lhsOallTpsFound :: Bool
                              _lhsOcomments :: ([String])
                              _lhsOdecls :: Decls
                              _lhsOerrors :: (Seq Error)
                              _lhsOsemNames :: ([String])
                              _hdOallNts :: (Set NontermIdent)
                              _hdOallPragmas :: PragmaMap
                              _hdOcontextMap :: ContextMap
                              _hdOerrors :: (Seq Error)
                              _hdOinh :: Attributes
                              _hdOnt :: NontermIdent
                              _hdOo_case :: Bool
                              _hdOo_cata :: Bool
                              _hdOo_costcentre :: Bool
                              _hdOo_data :: (Maybe Bool)
                              _hdOo_linePragmas :: Bool
                              _hdOo_newtypes :: Bool
                              _hdOo_pretty :: Bool
                              _hdOo_rename :: Bool
                              _hdOo_sem :: Bool
                              _hdOo_sig :: Bool
                              _hdOo_splitsems :: Bool
                              _hdOo_strictwrap :: Bool
                              _hdOo_traces :: Bool
                              _hdOo_unbox :: Bool
                              _hdOparamMap :: ParamMap
                              _hdOprefix :: String
                              _hdOsyn :: Attributes
                              _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                              _hdOwith_sig :: Bool
                              _tlOallNts :: (Set NontermIdent)
                              _tlOallPragmas :: PragmaMap
                              _tlOcontextMap :: ContextMap
                              _tlOerrors :: (Seq Error)
                              _tlOinh :: Attributes
                              _tlOnt :: NontermIdent
                              _tlOo_case :: Bool
                              _tlOo_cata :: Bool
                              _tlOo_costcentre :: Bool
                              _tlOo_data :: (Maybe Bool)
                              _tlOo_linePragmas :: Bool
                              _tlOo_newtypes :: Bool
                              _tlOo_pretty :: Bool
                              _tlOo_rename :: Bool
                              _tlOo_sem :: Bool
                              _tlOo_sig :: Bool
                              _tlOo_splitsems :: Bool
                              _tlOo_strictwrap :: Bool
                              _tlOo_traces :: Bool
                              _tlOo_unbox :: Bool
                              _tlOparamMap :: ParamMap
                              _tlOprefix :: String
                              _tlOsyn :: Attributes
                              _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                              _tlOwith_sig :: Bool
                              _hdIallTpsFound :: Bool
                              _hdIcataAlt :: Decl
                              _hdIcomments :: ([String])
                              _hdIdataAlt :: DataAlt
                              _hdIdecls :: Decls
                              _hdIerrors :: (Seq Error)
                              _hdIsemNames :: ([String])
                              _tlIallTpsFound :: Bool
                              _tlIcataAlts :: Decls
                              _tlIcomments :: ([String])
                              _tlIdataAlts :: DataAlts
                              _tlIdecls :: Decls
                              _tlIerrors :: (Seq Error)
                              _tlIsemNames :: ([String])
                              -- "GenerateCode.ag"(line 865, column 17)
                              _lhsOdataAlts =
                                  _hdIdataAlt : _tlIdataAlts
                              -- "GenerateCode.ag"(line 958, column 10)
                              _lhsOcataAlts =
                                  _hdIcataAlt : _tlIcataAlts
                              -- use rule "GenerateCode.ag"(line 706, column 39)
                              _lhsOallTpsFound =
                                  _hdIallTpsFound && _tlIallTpsFound
                              -- use rule "GenerateCode.ag"(line 738, column 52)
                              _lhsOcomments =
                                  _hdIcomments ++ _tlIcomments
                              -- use rule "GenerateCode.ag"(line 781, column 44)
                              _lhsOdecls =
                                  _hdIdecls ++ _tlIdecls
                              -- use rule "GenerateCode.ag"(line 719, column 32)
                              _lhsOerrors =
                                  _hdIerrors Seq.>< _tlIerrors
                              -- use rule "GenerateCode.ag"(line 977, column 61)
                              _lhsOsemNames =
                                  _hdIsemNames ++ _tlIsemNames
                              -- copy rule (down)
                              _hdOallNts =
                                  _lhsIallNts
                              -- copy rule (down)
                              _hdOallPragmas =
                                  _lhsIallPragmas
                              -- copy rule (down)
                              _hdOcontextMap =
                                  _lhsIcontextMap
                              -- copy rule (down)
                              _hdOerrors =
                                  _lhsIerrors
                              -- copy rule (down)
                              _hdOinh =
                                  _lhsIinh
                              -- copy rule (down)
                              _hdOnt =
                                  _lhsInt
                              -- copy rule (down)
                              _hdOo_case =
                                  _lhsIo_case
                              -- copy rule (down)
                              _hdOo_cata =
                                  _lhsIo_cata
                              -- copy rule (down)
                              _hdOo_costcentre =
                                  _lhsIo_costcentre
                              -- copy rule (down)
                              _hdOo_data =
                                  _lhsIo_data
                              -- copy rule (down)
                              _hdOo_linePragmas =
                                  _lhsIo_linePragmas
                              -- copy rule (down)
                              _hdOo_newtypes =
                                  _lhsIo_newtypes
                              -- copy rule (down)
                              _hdOo_pretty =
                                  _lhsIo_pretty
                              -- copy rule (down)
                              _hdOo_rename =
                                  _lhsIo_rename
                              -- copy rule (down)
                              _hdOo_sem =
                                  _lhsIo_sem
                              -- copy rule (down)
                              _hdOo_sig =
                                  _lhsIo_sig
                              -- copy rule (down)
                              _hdOo_splitsems =
                                  _lhsIo_splitsems
                              -- copy rule (down)
                              _hdOo_strictwrap =
                                  _lhsIo_strictwrap
                              -- copy rule (down)
                              _hdOo_traces =
                                  _lhsIo_traces
                              -- copy rule (down)
                              _hdOo_unbox =
                                  _lhsIo_unbox
                              -- copy rule (down)
                              _hdOparamMap =
                                  _lhsIparamMap
                              -- copy rule (down)
                              _hdOprefix =
                                  _lhsIprefix
                              -- copy rule (down)
                              _hdOsyn =
                                  _lhsIsyn
                              -- copy rule (down)
                              _hdOunfoldSemDom =
                                  _lhsIunfoldSemDom
                              -- copy rule (down)
                              _hdOwith_sig =
                                  _lhsIwith_sig
                              -- copy rule (down)
                              _tlOallNts =
                                  _lhsIallNts
                              -- copy rule (down)
                              _tlOallPragmas =
                                  _lhsIallPragmas
                              -- copy rule (down)
                              _tlOcontextMap =
                                  _lhsIcontextMap
                              -- copy rule (chain)
                              _tlOerrors =
                                  _hdIerrors
                              -- copy rule (down)
                              _tlOinh =
                                  _lhsIinh
                              -- copy rule (down)
                              _tlOnt =
                                  _lhsInt
                              -- copy rule (down)
                              _tlOo_case =
                                  _lhsIo_case
                              -- copy rule (down)
                              _tlOo_cata =
                                  _lhsIo_cata
                              -- copy rule (down)
                              _tlOo_costcentre =
                                  _lhsIo_costcentre
                              -- copy rule (down)
                              _tlOo_data =
                                  _lhsIo_data
                              -- copy rule (down)
                              _tlOo_linePragmas =
                                  _lhsIo_linePragmas
                              -- copy rule (down)
                              _tlOo_newtypes =
                                  _lhsIo_newtypes
                              -- copy rule (down)
                              _tlOo_pretty =
                                  _lhsIo_pretty
                              -- copy rule (down)
                              _tlOo_rename =
                                  _lhsIo_rename
                              -- copy rule (down)
                              _tlOo_sem =
                                  _lhsIo_sem
                              -- copy rule (down)
                              _tlOo_sig =
                                  _lhsIo_sig
                              -- copy rule (down)
                              _tlOo_splitsems =
                                  _lhsIo_splitsems
                              -- copy rule (down)
                              _tlOo_strictwrap =
                                  _lhsIo_strictwrap
                              -- copy rule (down)
                              _tlOo_traces =
                                  _lhsIo_traces
                              -- copy rule (down)
                              _tlOo_unbox =
                                  _lhsIo_unbox
                              -- copy rule (down)
                              _tlOparamMap =
                                  _lhsIparamMap
                              -- copy rule (down)
                              _tlOprefix =
                                  _lhsIprefix
                              -- copy rule (down)
                              _tlOsyn =
                                  _lhsIsyn
                              -- copy rule (down)
                              _tlOunfoldSemDom =
                                  _lhsIunfoldSemDom
                              -- copy rule (down)
                              _tlOwith_sig =
                                  _lhsIwith_sig
                              ( _hdIallTpsFound,_hdIcataAlt,_hdIcomments,_hdIdataAlt,_hdIdecls,_hdIerrors,_hdIsemNames) =
                                  (hd_ _hdOallNts _hdOallPragmas _hdOcontextMap _hdOerrors _hdOinh _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOparamMap _hdOprefix _hdOsyn _hdOunfoldSemDom _hdOwith_sig )
                              ( _tlIallTpsFound,_tlIcataAlts,_tlIcomments,_tlIdataAlts,_tlIdecls,_tlIerrors,_tlIsemNames) =
                                  (tl_ _tlOallNts _tlOallPragmas _tlOcontextMap _tlOerrors _tlOinh _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOparamMap _tlOprefix _tlOsyn _tlOunfoldSemDom _tlOwith_sig )
                          in  ( _lhsOallTpsFound,_lhsOcataAlts,_lhsOcomments,_lhsOdataAlts,_lhsOdecls,_lhsOerrors,_lhsOsemNames))) )
sem_CProductions_Nil :: T_CProductions 
sem_CProductions_Nil  =
    (T_CProductions (\ _lhsIallNts
                       _lhsIallPragmas
                       _lhsIcontextMap
                       _lhsIerrors
                       _lhsIinh
                       _lhsInt
                       _lhsIo_case
                       _lhsIo_cata
                       _lhsIo_costcentre
                       _lhsIo_data
                       _lhsIo_linePragmas
                       _lhsIo_newtypes
                       _lhsIo_pretty
                       _lhsIo_rename
                       _lhsIo_sem
                       _lhsIo_sig
                       _lhsIo_splitsems
                       _lhsIo_strictwrap
                       _lhsIo_traces
                       _lhsIo_unbox
                       _lhsIparamMap
                       _lhsIprefix
                       _lhsIsyn
                       _lhsIunfoldSemDom
                       _lhsIwith_sig ->
                         (let _lhsOdataAlts :: DataAlts
                              _lhsOcataAlts :: Decls
                              _lhsOallTpsFound :: Bool
                              _lhsOcomments :: ([String])
                              _lhsOdecls :: Decls
                              _lhsOerrors :: (Seq Error)
                              _lhsOsemNames :: ([String])
                              -- "GenerateCode.ag"(line 866, column 17)
                              _lhsOdataAlts =
                                  []
                              -- "GenerateCode.ag"(line 959, column 10)
                              _lhsOcataAlts =
                                  []
                              -- use rule "GenerateCode.ag"(line 706, column 39)
                              _lhsOallTpsFound =
                                  True
                              -- use rule "GenerateCode.ag"(line 738, column 52)
                              _lhsOcomments =
                                  []
                              -- use rule "GenerateCode.ag"(line 781, column 44)
                              _lhsOdecls =
                                  []
                              -- use rule "GenerateCode.ag"(line 719, column 32)
                              _lhsOerrors =
                                  Seq.empty
                              -- use rule "GenerateCode.ag"(line 977, column 61)
                              _lhsOsemNames =
                                  []
                          in  ( _lhsOallTpsFound,_lhsOcataAlts,_lhsOcomments,_lhsOdataAlts,_lhsOdecls,_lhsOerrors,_lhsOsemNames))) )
-- CRule -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         children             : [(Identifier,Type,Bool)]
         con                  : ConstructorIdent
         higherOrderChildren  : [(Identifier,Type,Bool)]
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         what                 : String
      chained attributes:
         declsAbove           : [Decl]
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         bldBlocksFun         : DeclBlocks -> DeclBlocks
         comments             : [String]
         decls                : Decls
         definedInsts         : [Identifier]
         exprs                : Exprs
         tSigs                : [Decl]
         tps                  : [Type]
         usedVars             : Set String
   alternatives:
      alternative CChildVisit:
         child name           : {Identifier}
         child nt             : {NontermIdent}
         child nr             : {Int}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child isLast         : {Bool}
         visit 0:
            local costCentreDescr : _
            local addCostCentre : _
            local decls       : _
            local isSuperfluousHigherOrderIntra : _
            local names       : _
            local mkTp        : _
            local definedTps  : _
            local nextTp      : _
            local orgParams   : _
            local instParams  : _
            local replParamMap : _
            local replace     : _
            local evalTp      : _
      alternative CRule:
         child name           : {Identifier}
         child isIn           : {Bool}
         child hasCode        : {Bool}
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child childnt        : {Maybe NontermIdent}
         child tp             : {Maybe Type}
         child pattern        : Pattern 
         child rhs            : {[String]}
         child defines        : {Map Int (Identifier,Identifier,Maybe Type)}
         child owrt           : {Bool}
         child origin         : {String}
         child uses           : {Set (Identifier, Identifier)}
         visit 0:
            local instTypes   : _
            local originComment : _
            local instDecls   : _
            local patDescr    : _
            local traceDescr  : _
            local addTrace    : _
            local costCentreDescr : _
            local addCostCentre : _
            local addLinePragma : _
            local decls       : _
            local definedInsts : _
            local rulename    : _
            local mkTp        : _
            local orgParams   : _
            local evalTp      : _
            local _tup3       : {([Type],Bool)}
-}
-- cata
sem_CRule :: CRule  ->
             T_CRule 
sem_CRule (CChildVisit _name _nt _nr _inh _syn _isLast )  =
    (sem_CRule_CChildVisit _name _nt _nr _inh _syn _isLast )
sem_CRule (CRule _name _isIn _hasCode _nt _con _field _childnt _tp _pattern _rhs _defines _owrt _origin _uses )  =
    (sem_CRule_CRule _name _isIn _hasCode _nt _con _field _childnt _tp (sem_Pattern _pattern ) _rhs _defines _owrt _origin _uses )
-- semantic domain
newtype T_CRule  = T_CRule ((Set NontermIdent) ->
                            ([(Identifier,Type,Bool)]) ->
                            ConstructorIdent ->
                            ([Decl]) ->
                            (Seq Error) ->
                            ([(Identifier,Type,Bool)]) ->
                            Attributes ->
                            (Map Identifier Int) ->
                            Int ->
                            NontermIdent ->
                            Bool ->
                            Bool ->
                            Bool ->
                            (Maybe Bool) ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            Bool ->
                            (Map Identifier (NontermIdent, [String])) ->
                            ParamMap ->
                            String ->
                            Attributes ->
                            ([Identifier]) ->
                            (NontermIdent -> Int -> [String] -> Code.Type) ->
                            String ->
                            ( Bool,(DeclBlocks -> DeclBlocks),([String]),Decls,([Decl]),([Identifier]),(Seq Error),Exprs,([Decl]),([Type]),(Set String)))
data Inh_CRule  = Inh_CRule {allNts_Inh_CRule :: !(Set NontermIdent),children_Inh_CRule :: !([(Identifier,Type,Bool)]),con_Inh_CRule :: !(ConstructorIdent),declsAbove_Inh_CRule :: !([Decl]),errors_Inh_CRule :: !(Seq Error),higherOrderChildren_Inh_CRule :: !([(Identifier,Type,Bool)]),inh_Inh_CRule :: !(Attributes),instVisitNrs_Inh_CRule :: !(Map Identifier Int),nr_Inh_CRule :: !(Int),nt_Inh_CRule :: !(NontermIdent),o_case_Inh_CRule :: !(Bool),o_cata_Inh_CRule :: !(Bool),o_costcentre_Inh_CRule :: !(Bool),o_data_Inh_CRule :: !(Maybe Bool),o_linePragmas_Inh_CRule :: !(Bool),o_newtypes_Inh_CRule :: !(Bool),o_pretty_Inh_CRule :: !(Bool),o_rename_Inh_CRule :: !(Bool),o_sem_Inh_CRule :: !(Bool),o_sig_Inh_CRule :: !(Bool),o_splitsems_Inh_CRule :: !(Bool),o_strictwrap_Inh_CRule :: !(Bool),o_traces_Inh_CRule :: !(Bool),o_unbox_Inh_CRule :: !(Bool),paramInstMap_Inh_CRule :: !(Map Identifier (NontermIdent, [String])),paramMap_Inh_CRule :: !(ParamMap),prefix_Inh_CRule :: !(String),syn_Inh_CRule :: !(Attributes),terminals_Inh_CRule :: !([Identifier]),unfoldSemDom_Inh_CRule :: !(NontermIdent -> Int -> [String] -> Code.Type),what_Inh_CRule :: !(String)}
data Syn_CRule  = Syn_CRule {allTpsFound_Syn_CRule :: !(Bool),bldBlocksFun_Syn_CRule :: !(DeclBlocks -> DeclBlocks),comments_Syn_CRule :: !([String]),decls_Syn_CRule :: !(Decls),declsAbove_Syn_CRule :: !([Decl]),definedInsts_Syn_CRule :: !([Identifier]),errors_Syn_CRule :: !(Seq Error),exprs_Syn_CRule :: !(Exprs),tSigs_Syn_CRule :: !([Decl]),tps_Syn_CRule :: !([Type]),usedVars_Syn_CRule :: !(Set String)}
wrap_CRule :: T_CRule  ->
              Inh_CRule  ->
              Syn_CRule 
wrap_CRule (T_CRule sem ) (Inh_CRule _lhsIallNts _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIerrors _lhsIhigherOrderChildren _lhsIinh _lhsIinstVisitNrs _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwhat )  =
    (let ( _lhsOallTpsFound,_lhsObldBlocksFun,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOerrors,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars) =
             (sem _lhsIallNts _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIerrors _lhsIhigherOrderChildren _lhsIinh _lhsIinstVisitNrs _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwhat )
     in  (Syn_CRule _lhsOallTpsFound _lhsObldBlocksFun _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOerrors _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars ))
sem_CRule_CChildVisit :: Identifier ->
                         NontermIdent ->
                         Int ->
                         Attributes ->
                         Attributes ->
                         Bool ->
                         T_CRule 
sem_CRule_CChildVisit name_ nt_ nr_ inh_ syn_ isLast_  =
    (T_CRule (\ _lhsIallNts
                _lhsIchildren
                _lhsIcon
                _lhsIdeclsAbove
                _lhsIerrors
                _lhsIhigherOrderChildren
                _lhsIinh
                _lhsIinstVisitNrs
                _lhsInr
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_costcentre
                _lhsIo_data
                _lhsIo_linePragmas
                _lhsIo_newtypes
                _lhsIo_pretty
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_splitsems
                _lhsIo_strictwrap
                _lhsIo_traces
                _lhsIo_unbox
                _lhsIparamInstMap
                _lhsIparamMap
                _lhsIprefix
                _lhsIsyn
                _lhsIterminals
                _lhsIunfoldSemDom
                _lhsIwhat ->
                  (let _lhsOexprs :: Exprs
                       _lhsOusedVars :: (Set String)
                       _lhsOtSigs :: ([Decl])
                       _lhsOtps :: ([Type])
                       _lhsOdeclsAbove :: ([Decl])
                       _lhsObldBlocksFun :: (DeclBlocks -> DeclBlocks)
                       _lhsOallTpsFound :: Bool
                       _lhsOcomments :: ([String])
                       _lhsOdecls :: Decls
                       _lhsOdefinedInsts :: ([Identifier])
                       _lhsOerrors :: (Seq Error)
                       -- "GenerateCode.ag"(line 181, column 18)
                       _costCentreDescr =
                           show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show name_ ++ ":" ++ show nt_ ++ ":" ++ show nr_
                       -- "GenerateCode.ag"(line 182, column 18)
                       _addCostCentre =
                           \v -> if _lhsIo_costcentre
                                 then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                 else v
                       -- "GenerateCode.ag"(line 185, column 18)
                       _decls =
                           let  lhsVars =  map (attrname True name_) (Map.keys syn_)
                                           ++ if isLast_ then [] else [unwrap ++ funname name_ (nr_+1)]
                                rhsVars = map (attrname False name_) (Map.keys inh_)
                                unwrap = if _lhsIo_newtypes then typeName nt_ (nr_ + 1) ++ " " else ""
                                tuple = mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars
                                rhs = _addCostCentre     $ App (funname name_ nr_) (map SimpleExpr rhsVars)
                           in [Decl tuple rhs (Set.fromList lhsVars) (Set.fromList (funname name_ nr_ : rhsVars))]
                       -- "GenerateCode.ag"(line 261, column 7)
                       _isSuperfluousHigherOrderIntra =
                           _lhsInr <= Map.findWithDefault (-1) name_ _lhsIinstVisitNrs
                       -- "GenerateCode.ag"(line 275, column 8)
                       _names =
                           if _isSuperfluousHigherOrderIntra
                           then []
                           else [funname name_ (nr_+1)]
                       -- "GenerateCode.ag"(line 279, column 8)
                       _lhsOexprs =
                           let wrap = if _lhsIo_newtypes then \x -> App (typeName nt_ (nr_ + 1)) [x] else id
                               addType expr | null _instParams     = expr
                                            | otherwise            = TypedExpr expr (_lhsIunfoldSemDom nt_ (nr_+1) _instParams    )
                           in map (wrap . addType . SimpleExpr) _names
                       -- "GenerateCode.ag"(line 291, column 7)
                       _lhsOusedVars =
                           Set.fromList _names
                       -- "GenerateCode.ag"(line 315, column 19)
                       _mkTp =
                           _evalTp     . SimpleType . typeToHaskellString (Just nt_) _orgParams
                       -- "GenerateCode.ag"(line 316, column 19)
                       _definedTps =
                           [ TSig (attrname True name_ a) (_mkTp tp) |  (a,tp) <- Map.toList syn_ ]
                       -- "GenerateCode.ag"(line 317, column 19)
                       _nextTp =
                           typeName nt_ (nr_+1)
                       -- "GenerateCode.ag"(line 318, column 19)
                       _lhsOtSigs =
                           (if isLast_ then id else (TSig (funname name_ (nr_+1)) (TypeApp (SimpleType _nextTp) (map SimpleType _instParams    )) :)) _definedTps
                       -- "GenerateCode.ag"(line 320, column 19)
                       _orgParams =
                           map getName $ Map.findWithDefault [] nt_ _lhsIparamMap
                       -- "GenerateCode.ag"(line 321, column 19)
                       _instParams =
                           snd $ Map.findWithDefault (nt_,[]) name_ _lhsIparamInstMap
                       -- "GenerateCode.ag"(line 322, column 19)
                       _replParamMap =
                           Map.fromList (zip _orgParams     _instParams    )
                       -- "GenerateCode.ag"(line 323, column 19)
                       _replace =
                           \k -> Map.findWithDefault k k _replParamMap
                       -- "GenerateCode.ag"(line 324, column 19)
                       _evalTp =
                           if null _orgParams     then id else evalType _replace
                       -- "GenerateCode.ag"(line 347, column 19)
                       _lhsOtps =
                           if _isSuperfluousHigherOrderIntra
                           then []
                           else [NT (ntOfVisit nt_ (nr_+1)) _instParams    ]
                       -- "GenerateCode.ag"(line 472, column 7)
                       _lhsOdeclsAbove =
                           []
                       -- "GenerateCode.ag"(line 485, column 7)
                       _lhsObldBlocksFun =
                           DeclBlock _lhsIdeclsAbove (head _decls    )
                       -- use rule "GenerateCode.ag"(line 344, column 39)
                       _lhsOallTpsFound =
                           True
                       -- use rule "GenerateCode.ag"(line 738, column 52)
                       _lhsOcomments =
                           []
                       -- use rule "GenerateCode.ag"(line 138, column 34)
                       _lhsOdecls =
                           _decls
                       -- use rule "GenerateCode.ag"(line 195, column 55)
                       _lhsOdefinedInsts =
                           []
                       -- use rule "GenerateCode.ag"(line 719, column 32)
                       _lhsOerrors =
                           Seq.empty
                   in  ( _lhsOallTpsFound,_lhsObldBlocksFun,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOerrors,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars))) )
sem_CRule_CRule :: Identifier ->
                   Bool ->
                   Bool ->
                   NontermIdent ->
                   ConstructorIdent ->
                   Identifier ->
                   (Maybe NontermIdent) ->
                   (Maybe Type) ->
                   T_Pattern  ->
                   ([String]) ->
                   (Map Int (Identifier,Identifier,Maybe Type)) ->
                   Bool ->
                   String ->
                   (Set (Identifier, Identifier)) ->
                   T_CRule 
sem_CRule_CRule name_ isIn_ hasCode_ nt_ con_ field_ childnt_ tp_ (T_Pattern pattern_ ) rhs_ defines_ owrt_ origin_ uses_  =
    (T_CRule (\ _lhsIallNts
                _lhsIchildren
                _lhsIcon
                _lhsIdeclsAbove
                _lhsIerrors
                _lhsIhigherOrderChildren
                _lhsIinh
                _lhsIinstVisitNrs
                _lhsInr
                _lhsInt
                _lhsIo_case
                _lhsIo_cata
                _lhsIo_costcentre
                _lhsIo_data
                _lhsIo_linePragmas
                _lhsIo_newtypes
                _lhsIo_pretty
                _lhsIo_rename
                _lhsIo_sem
                _lhsIo_sig
                _lhsIo_splitsems
                _lhsIo_strictwrap
                _lhsIo_traces
                _lhsIo_unbox
                _lhsIparamInstMap
                _lhsIparamMap
                _lhsIprefix
                _lhsIsyn
                _lhsIterminals
                _lhsIunfoldSemDom
                _lhsIwhat ->
                  (let _lhsOexprs :: Exprs
                       _lhsOusedVars :: (Set String)
                       _lhsOtSigs :: ([Decl])
                       __tup3 :: (([Type],Bool))
                       _lhsOtps :: ([Type])
                       _lhsOallTpsFound :: Bool
                       _lhsOdeclsAbove :: ([Decl])
                       _lhsObldBlocksFun :: (DeclBlocks -> DeclBlocks)
                       _lhsOerrors :: (Seq Error)
                       _lhsOcomments :: ([String])
                       _lhsOdecls :: Decls
                       _lhsOdefinedInsts :: ([Identifier])
                       _patternIcopy :: Pattern
                       _patternIdefinedInsts :: ([Identifier])
                       _patternIpatternAttributes :: ([(Identifier, Identifier)])
                       -- "GenerateCode.ag"(line 140, column 12)
                       _instTypes =
                           map (\(n,NT t _,_) -> (n,t)) _lhsIhigherOrderChildren
                       -- "GenerateCode.ag"(line 141, column 12)
                       _originComment =
                           if  _lhsIo_pretty
                               then (Comment origin_:)
                               else id
                       -- "GenerateCode.ag"(line 144, column 12)
                       _instDecls =
                           [ Decl (Pattern3 (Alias _INST' inst (Underscore (getPos inst)) []))
                                  ( let nm = fromJust $ inst `lookup` _instTypes
                                    in case nm `Set.member` _lhsIallNts of
                                         True  -> App (cataname _lhsIprefix nm)
                                                      [SimpleExpr instLocFieldName]
                                         False -> SimpleExpr instLocFieldName
                                  )
                                  (Set.singleton instSemFieldName)
                                  (Set.singleton instLocFieldName)
                           | inst <- _definedInsts
                           , let instLocFieldName = attrname True _INST inst
                           , let instSemFieldName = attrname False _INST' inst ]
                       -- "GenerateCode.ag"(line 156, column 12)
                       _patDescr =
                           if isIn_
                           then "_"
                           else concat $ intersperse "," (map (\(f,a) -> show f ++ "." ++ show a) _patternIpatternAttributes)
                       -- "GenerateCode.ag"(line 159, column 12)
                       _traceDescr =
                           show nt_ ++ " :: " ++ show con_ ++ " :: " ++ _patDescr
                       -- "GenerateCode.ag"(line 161, column 12)
                       _addTrace =
                           \v -> if _lhsIo_traces
                                 then Trace _traceDescr     v
                                 else v
                       -- "GenerateCode.ag"(line 164, column 12)
                       _costCentreDescr =
                           show nt_ ++ ":" ++ show con_ ++ ":" ++ _patDescr
                       -- "GenerateCode.ag"(line 165, column 12)
                       _addCostCentre =
                           \v -> if _lhsIo_costcentre
                                 then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                 else v
                       -- "GenerateCode.ag"(line 168, column 12)
                       _addLinePragma =
                           \v -> let p = getPos name_
                                     hasPos = line p > 0 && column p >= 0 && not (null (file p))
                                 in if _lhsIo_linePragmas && hasPos
                                    then PragmaExpr True True ("LINE " ++ show (line p) ++ " " ++ show (file p))
                                         $ LineExpr
                                         $ v
                                    else v
                       -- "GenerateCode.ag"(line 175, column 12)
                       _decls =
                           if hasCode_
                           then _originComment ( Decl (Pattern3 _patternIcopy) (_addTrace     $ _addCostCentre     $ _addLinePragma     $ (TextExpr rhs_))
                                                      (Set.fromList [attrname False fld nm | (fld,nm,_) <- Map.elems defines_])
                                                      (Set.fromList [attrname True fld nm | (fld,nm) <- Set.toList uses_])
                                               : _instDecls    )
                           else _instDecls
                       -- "GenerateCode.ag"(line 200, column 12)
                       _definedInsts =
                           if isIn_ then [] else _patternIdefinedInsts
                       -- "GenerateCode.ag"(line 270, column 12)
                       _rulename =
                           if  field_ == _LOC && name_ `elem` _lhsIterminals
                           then funname name_ 0
                           else attrname isIn_ field_ name_
                       -- "GenerateCode.ag"(line 273, column 12)
                       _lhsOexprs =
                           [SimpleExpr _rulename    ]
                       -- "GenerateCode.ag"(line 289, column 7)
                       _lhsOusedVars =
                           Set.singleton _rulename
                       -- "GenerateCode.ag"(line 299, column 19)
                       _mkTp =
                           SimpleType . typeToHaskellString (Just _lhsInt) _orgParams
                       -- "GenerateCode.ag"(line 300, column 19)
                       _lhsOtSigs =
                           [ TSig (attrname False field attr) (_evalTp     field $ _mkTp (fromJust tp))
                           |  (field,attr,tp) <- Map.elems defines_, isJust tp ]
                       -- "GenerateCode.ag"(line 303, column 19)
                       _orgParams =
                           map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                       -- "GenerateCode.ag"(line 304, column 19)
                       _evalTp =
                           \field tp -> let orgFldParams = map getName $ Map.findWithDefault [] childNt _lhsIparamMap
                                            (childNt,instParams) = Map.findWithDefault (_lhsInt,[]) field _lhsIparamInstMap
                                            replMap = Map.fromList (zip orgFldParams instParams)
                                            replace k = Map.findWithDefault ('@':k) k replMap
                                        in if null instParams
                                           then if null _orgParams
                                                then tp
                                                else idEvalType tp
                                           else evalType replace tp
                       -- "GenerateCode.ag"(line 346, column 23)
                       __tup3 =
                           maybe ([],False) (\tp -> ([substSelf (fromJust childnt_) tp],True)) tp_
                       -- "GenerateCode.ag"(line 346, column 23)
                       (_lhsOtps,_) =
                           __tup3
                       -- "GenerateCode.ag"(line 346, column 23)
                       (_,_lhsOallTpsFound) =
                           __tup3
                       -- "GenerateCode.ag"(line 470, column 7)
                       _lhsOdeclsAbove =
                           _lhsIdeclsAbove ++ _decls
                       -- "GenerateCode.ag"(line 483, column 7)
                       _lhsObldBlocksFun =
                           id
                       -- "GenerateCode.ag"(line 725, column 12)
                       _lhsOerrors =
                           let nameOf (Alias _ _ _ pats@(_:_)) = Ident (show (map (\(Alias _ a _ _)->a) pats)) (getPos name_)
                               nameOf _ = name_
                           in  maybe (Seq.singleton (MissingTypeSig _lhsInt _lhsIcon (nameOf _patternIcopy))) (const Seq.empty) tp_
                       -- "GenerateCode.ag"(line 765, column 18)
                       _lhsOcomments =
                           [ makeLocalComment 11 _lhsIwhat name tp | (field,name,tp) <- Map.elems defines_, field == _LOC ]
                           ++ [ makeLocalComment 11 "inst " name tp | (field,name,tp) <- Map.elems defines_, field == _INST ]
                       -- use rule "GenerateCode.ag"(line 138, column 34)
                       _lhsOdecls =
                           _decls
                       -- use rule "GenerateCode.ag"(line 195, column 55)
                       _lhsOdefinedInsts =
                           _definedInsts
                       ( _patternIcopy,_patternIdefinedInsts,_patternIpatternAttributes) =
                           (pattern_ )
                   in  ( _lhsOallTpsFound,_lhsObldBlocksFun,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOerrors,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars))) )
-- CSegment ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inh                  : Attributes
         isLast               : Bool
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
      synthesized attributes:
         comments             : [String]
         semDom               : [Decl]
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
         wrapDecls            : Decls
   alternatives:
      alternative CSegment:
         child inh            : {Attributes}
         child syn            : {Attributes}
         visit 0:
            local tp          : _
            local inhTps      : _
            local synTps      : _
            local continuation : _
            local params      : _
-}
-- cata
sem_CSegment :: CSegment  ->
                T_CSegment 
sem_CSegment (CSegment _inh _syn )  =
    (sem_CSegment_CSegment _inh _syn )
-- semantic domain
newtype T_CSegment  = T_CSegment (Attributes ->
                                  Bool ->
                                  Int ->
                                  NontermIdent ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  (Maybe Bool) ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  ParamMap ->
                                  String ->
                                  Attributes ->
                                  ( ([String]),([Decl]),(Map (NontermIdent, Int) ([String], Code.Type)),Decls))
data Inh_CSegment  = Inh_CSegment {inh_Inh_CSegment :: !(Attributes),isLast_Inh_CSegment :: !(Bool),nr_Inh_CSegment :: !(Int),nt_Inh_CSegment :: !(NontermIdent),o_case_Inh_CSegment :: !(Bool),o_cata_Inh_CSegment :: !(Bool),o_costcentre_Inh_CSegment :: !(Bool),o_data_Inh_CSegment :: !(Maybe Bool),o_linePragmas_Inh_CSegment :: !(Bool),o_newtypes_Inh_CSegment :: !(Bool),o_pretty_Inh_CSegment :: !(Bool),o_rename_Inh_CSegment :: !(Bool),o_sem_Inh_CSegment :: !(Bool),o_sig_Inh_CSegment :: !(Bool),o_splitsems_Inh_CSegment :: !(Bool),o_strictwrap_Inh_CSegment :: !(Bool),o_traces_Inh_CSegment :: !(Bool),o_unbox_Inh_CSegment :: !(Bool),paramMap_Inh_CSegment :: !(ParamMap),prefix_Inh_CSegment :: !(String),syn_Inh_CSegment :: !(Attributes)}
data Syn_CSegment  = Syn_CSegment {comments_Syn_CSegment :: !([String]),semDom_Syn_CSegment :: !([Decl]),semDomUnfoldGath_Syn_CSegment :: !(Map (NontermIdent, Int) ([String], Code.Type)),wrapDecls_Syn_CSegment :: !(Decls)}
wrap_CSegment :: T_CSegment  ->
                 Inh_CSegment  ->
                 Syn_CSegment 
wrap_CSegment (T_CSegment sem ) (Inh_CSegment _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn )  =
    (let ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls) =
             (sem _lhsIinh _lhsIisLast _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn )
     in  (Syn_CSegment _lhsOcomments _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls ))
sem_CSegment_CSegment :: Attributes ->
                         Attributes ->
                         T_CSegment 
sem_CSegment_CSegment inh_ syn_  =
    (T_CSegment (\ _lhsIinh
                   _lhsIisLast
                   _lhsInr
                   _lhsInt
                   _lhsIo_case
                   _lhsIo_cata
                   _lhsIo_costcentre
                   _lhsIo_data
                   _lhsIo_linePragmas
                   _lhsIo_newtypes
                   _lhsIo_pretty
                   _lhsIo_rename
                   _lhsIo_sem
                   _lhsIo_sig
                   _lhsIo_splitsems
                   _lhsIo_strictwrap
                   _lhsIo_traces
                   _lhsIo_unbox
                   _lhsIparamMap
                   _lhsIprefix
                   _lhsIsyn ->
                     (let _lhsOsemDom :: ([Decl])
                          _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                          _lhsOwrapDecls :: Decls
                          _lhsOcomments :: ([String])
                          -- "GenerateCode.ag"(line 597, column 15)
                          _tp =
                              foldr Arr _synTps     _inhTps
                          -- "GenerateCode.ag"(line 598, column 15)
                          _inhTps =
                              [SimpleType (typeToHaskellString (Just _lhsInt) _params     tp) |  tp <- Map.elems inh_]
                          -- "GenerateCode.ag"(line 599, column 15)
                          _synTps =
                              mkTupleType _lhsIo_unbox (null _inhTps    ) ([SimpleType (typeToHaskellString (Just _lhsInt) _params     tp) |  tp <- Map.elems syn_] ++ _continuation    )
                          -- "GenerateCode.ag"(line 600, column 15)
                          _continuation =
                              if  _lhsIisLast
                              then []
                              else [TypeApp (SimpleType (typeName _lhsInt (_lhsInr + 1))) (map (SimpleType . ('@':)) _params    )]
                          -- "GenerateCode.ag"(line 603, column 15)
                          _params =
                              map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                          -- "GenerateCode.ag"(line 604, column 15)
                          _lhsOsemDom =
                              let name = typeName _lhsInt _lhsInr
                                  evalTp | null _params     = id
                                         | otherwise        = idEvalType
                              in if _lhsIo_newtypes
                                 then [ Code.NewType name _params     name (evalTp _tp    ) ]
                                 else [ Code.Type name _params     (evalTp _tp    ) ]
                          -- "GenerateCode.ag"(line 615, column 7)
                          _lhsOsemDomUnfoldGath =
                              Map.singleton (_lhsInt, _lhsInr) (_params    , _tp    )
                          -- "GenerateCode.ag"(line 689, column 15)
                          _lhsOwrapDecls =
                              let lhsVars = map (lhsname False) (Map.keys syn_)
                                            ++ if _lhsIisLast then [] else [unwrap ++ sem (_lhsInr+1)]
                                  rhsVars = map (lhsname True) (Map.keys inh_)
                                  rhs = map SimpleExpr rhsVars
                                  unwrap = if _lhsIo_newtypes then typeName _lhsInt (_lhsInr + 1) ++ " " else ""
                                  var = "sem"
                                  sem 0 = var
                                  sem n = var ++ "_" ++ show n
                              in [ Decl (mkTupleLhs _lhsIo_unbox (null $ Map.keys inh_) lhsVars) (App (sem _lhsInr) rhs) (Set.fromList lhsVars) (Set.fromList rhsVars) ]
                          -- "GenerateCode.ag"(line 743, column 18)
                          _lhsOcomments =
                              let body = map ind (showsSegment (CSegment inh_ syn_))
                              in if null body
                                 then []
                                 else ("visit " ++ show _lhsInr ++ ":") : body
                      in  ( _lhsOcomments,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))) )
-- CSegments ---------------------------------------------------
{-
   visit 0:
      inherited attributes:
         inh                  : Attributes
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
      synthesized attributes:
         comments             : [String]
         isNil                : Bool
         semDom               : [Decl]
         semDomUnfoldGath     : Map (NontermIdent, Int) ([String], Code.Type)
         wrapDecls            : Decls
   alternatives:
      alternative Cons:
         child hd             : CSegment 
         child tl             : CSegments 
      alternative Nil:
-}
-- cata
sem_CSegments :: CSegments  ->
                 T_CSegments 
sem_CSegments list  =
    (Prelude.foldr sem_CSegments_Cons sem_CSegments_Nil (Prelude.map sem_CSegment list) )
-- semantic domain
newtype T_CSegments  = T_CSegments (Attributes ->
                                    Int ->
                                    NontermIdent ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    (Maybe Bool) ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    Bool ->
                                    ParamMap ->
                                    String ->
                                    Attributes ->
                                    ( ([String]),Bool,([Decl]),(Map (NontermIdent, Int) ([String], Code.Type)),Decls))
data Inh_CSegments  = Inh_CSegments {inh_Inh_CSegments :: !(Attributes),nr_Inh_CSegments :: !(Int),nt_Inh_CSegments :: !(NontermIdent),o_case_Inh_CSegments :: !(Bool),o_cata_Inh_CSegments :: !(Bool),o_costcentre_Inh_CSegments :: !(Bool),o_data_Inh_CSegments :: !(Maybe Bool),o_linePragmas_Inh_CSegments :: !(Bool),o_newtypes_Inh_CSegments :: !(Bool),o_pretty_Inh_CSegments :: !(Bool),o_rename_Inh_CSegments :: !(Bool),o_sem_Inh_CSegments :: !(Bool),o_sig_Inh_CSegments :: !(Bool),o_splitsems_Inh_CSegments :: !(Bool),o_strictwrap_Inh_CSegments :: !(Bool),o_traces_Inh_CSegments :: !(Bool),o_unbox_Inh_CSegments :: !(Bool),paramMap_Inh_CSegments :: !(ParamMap),prefix_Inh_CSegments :: !(String),syn_Inh_CSegments :: !(Attributes)}
data Syn_CSegments  = Syn_CSegments {comments_Syn_CSegments :: !([String]),isNil_Syn_CSegments :: !(Bool),semDom_Syn_CSegments :: !([Decl]),semDomUnfoldGath_Syn_CSegments :: !(Map (NontermIdent, Int) ([String], Code.Type)),wrapDecls_Syn_CSegments :: !(Decls)}
wrap_CSegments :: T_CSegments  ->
                  Inh_CSegments  ->
                  Syn_CSegments 
wrap_CSegments (T_CSegments sem ) (Inh_CSegments _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn )  =
    (let ( _lhsOcomments,_lhsOisNil,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls) =
             (sem _lhsIinh _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamMap _lhsIprefix _lhsIsyn )
     in  (Syn_CSegments _lhsOcomments _lhsOisNil _lhsOsemDom _lhsOsemDomUnfoldGath _lhsOwrapDecls ))
sem_CSegments_Cons :: T_CSegment  ->
                      T_CSegments  ->
                      T_CSegments 
sem_CSegments_Cons (T_CSegment hd_ ) (T_CSegments tl_ )  =
    (T_CSegments (\ _lhsIinh
                    _lhsInr
                    _lhsInt
                    _lhsIo_case
                    _lhsIo_cata
                    _lhsIo_costcentre
                    _lhsIo_data
                    _lhsIo_linePragmas
                    _lhsIo_newtypes
                    _lhsIo_pretty
                    _lhsIo_rename
                    _lhsIo_sem
                    _lhsIo_sig
                    _lhsIo_splitsems
                    _lhsIo_strictwrap
                    _lhsIo_traces
                    _lhsIo_unbox
                    _lhsIparamMap
                    _lhsIprefix
                    _lhsIsyn ->
                      (let _tlOnr :: Int
                           _lhsOisNil :: Bool
                           _hdOisLast :: Bool
                           _lhsOcomments :: ([String])
                           _lhsOsemDom :: ([Decl])
                           _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _lhsOwrapDecls :: Decls
                           _hdOinh :: Attributes
                           _hdOnr :: Int
                           _hdOnt :: NontermIdent
                           _hdOo_case :: Bool
                           _hdOo_cata :: Bool
                           _hdOo_costcentre :: Bool
                           _hdOo_data :: (Maybe Bool)
                           _hdOo_linePragmas :: Bool
                           _hdOo_newtypes :: Bool
                           _hdOo_pretty :: Bool
                           _hdOo_rename :: Bool
                           _hdOo_sem :: Bool
                           _hdOo_sig :: Bool
                           _hdOo_splitsems :: Bool
                           _hdOo_strictwrap :: Bool
                           _hdOo_traces :: Bool
                           _hdOo_unbox :: Bool
                           _hdOparamMap :: ParamMap
                           _hdOprefix :: String
                           _hdOsyn :: Attributes
                           _tlOinh :: Attributes
                           _tlOnt :: NontermIdent
                           _tlOo_case :: Bool
                           _tlOo_cata :: Bool
                           _tlOo_costcentre :: Bool
                           _tlOo_data :: (Maybe Bool)
                           _tlOo_linePragmas :: Bool
                           _tlOo_newtypes :: Bool
                           _tlOo_pretty :: Bool
                           _tlOo_rename :: Bool
                           _tlOo_sem :: Bool
                           _tlOo_sig :: Bool
                           _tlOo_splitsems :: Bool
                           _tlOo_strictwrap :: Bool
                           _tlOo_traces :: Bool
                           _tlOo_unbox :: Bool
                           _tlOparamMap :: ParamMap
                           _tlOprefix :: String
                           _tlOsyn :: Attributes
                           _hdIcomments :: ([String])
                           _hdIsemDom :: ([Decl])
                           _hdIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _hdIwrapDecls :: Decls
                           _tlIcomments :: ([String])
                           _tlIisNil :: Bool
                           _tlIsemDom :: ([Decl])
                           _tlIsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _tlIwrapDecls :: Decls
                           -- "GenerateCode.ag"(line 220, column 11)
                           _tlOnr =
                               _lhsInr + 1
                           -- "GenerateCode.ag"(line 233, column 12)
                           _lhsOisNil =
                               False
                           -- "GenerateCode.ag"(line 234, column 12)
                           _hdOisLast =
                               _tlIisNil
                           -- use rule "GenerateCode.ag"(line 738, column 52)
                           _lhsOcomments =
                               _hdIcomments ++ _tlIcomments
                           -- use rule "GenerateCode.ag"(line 592, column 50)
                           _lhsOsemDom =
                               _hdIsemDom ++ _tlIsemDom
                           -- use rule "GenerateCode.ag"(line 611, column 86)
                           _lhsOsemDomUnfoldGath =
                               _hdIsemDomUnfoldGath `Map.union` _tlIsemDomUnfoldGath
                           -- use rule "GenerateCode.ag"(line 687, column 52)
                           _lhsOwrapDecls =
                               _hdIwrapDecls ++ _tlIwrapDecls
                           -- copy rule (down)
                           _hdOinh =
                               _lhsIinh
                           -- copy rule (down)
                           _hdOnr =
                               _lhsInr
                           -- copy rule (down)
                           _hdOnt =
                               _lhsInt
                           -- copy rule (down)
                           _hdOo_case =
                               _lhsIo_case
                           -- copy rule (down)
                           _hdOo_cata =
                               _lhsIo_cata
                           -- copy rule (down)
                           _hdOo_costcentre =
                               _lhsIo_costcentre
                           -- copy rule (down)
                           _hdOo_data =
                               _lhsIo_data
                           -- copy rule (down)
                           _hdOo_linePragmas =
                               _lhsIo_linePragmas
                           -- copy rule (down)
                           _hdOo_newtypes =
                               _lhsIo_newtypes
                           -- copy rule (down)
                           _hdOo_pretty =
                               _lhsIo_pretty
                           -- copy rule (down)
                           _hdOo_rename =
                               _lhsIo_rename
                           -- copy rule (down)
                           _hdOo_sem =
                               _lhsIo_sem
                           -- copy rule (down)
                           _hdOo_sig =
                               _lhsIo_sig
                           -- copy rule (down)
                           _hdOo_splitsems =
                               _lhsIo_splitsems
                           -- copy rule (down)
                           _hdOo_strictwrap =
                               _lhsIo_strictwrap
                           -- copy rule (down)
                           _hdOo_traces =
                               _lhsIo_traces
                           -- copy rule (down)
                           _hdOo_unbox =
                               _lhsIo_unbox
                           -- copy rule (down)
                           _hdOparamMap =
                               _lhsIparamMap
                           -- copy rule (down)
                           _hdOprefix =
                               _lhsIprefix
                           -- copy rule (down)
                           _hdOsyn =
                               _lhsIsyn
                           -- copy rule (down)
                           _tlOinh =
                               _lhsIinh
                           -- copy rule (down)
                           _tlOnt =
                               _lhsInt
                           -- copy rule (down)
                           _tlOo_case =
                               _lhsIo_case
                           -- copy rule (down)
                           _tlOo_cata =
                               _lhsIo_cata
                           -- copy rule (down)
                           _tlOo_costcentre =
                               _lhsIo_costcentre
                           -- copy rule (down)
                           _tlOo_data =
                               _lhsIo_data
                           -- copy rule (down)
                           _tlOo_linePragmas =
                               _lhsIo_linePragmas
                           -- copy rule (down)
                           _tlOo_newtypes =
                               _lhsIo_newtypes
                           -- copy rule (down)
                           _tlOo_pretty =
                               _lhsIo_pretty
                           -- copy rule (down)
                           _tlOo_rename =
                               _lhsIo_rename
                           -- copy rule (down)
                           _tlOo_sem =
                               _lhsIo_sem
                           -- copy rule (down)
                           _tlOo_sig =
                               _lhsIo_sig
                           -- copy rule (down)
                           _tlOo_splitsems =
                               _lhsIo_splitsems
                           -- copy rule (down)
                           _tlOo_strictwrap =
                               _lhsIo_strictwrap
                           -- copy rule (down)
                           _tlOo_traces =
                               _lhsIo_traces
                           -- copy rule (down)
                           _tlOo_unbox =
                               _lhsIo_unbox
                           -- copy rule (down)
                           _tlOparamMap =
                               _lhsIparamMap
                           -- copy rule (down)
                           _tlOprefix =
                               _lhsIprefix
                           -- copy rule (down)
                           _tlOsyn =
                               _lhsIsyn
                           ( _hdIcomments,_hdIsemDom,_hdIsemDomUnfoldGath,_hdIwrapDecls) =
                               (hd_ _hdOinh _hdOisLast _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOparamMap _hdOprefix _hdOsyn )
                           ( _tlIcomments,_tlIisNil,_tlIsemDom,_tlIsemDomUnfoldGath,_tlIwrapDecls) =
                               (tl_ _tlOinh _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOparamMap _tlOprefix _tlOsyn )
                       in  ( _lhsOcomments,_lhsOisNil,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))) )
sem_CSegments_Nil :: T_CSegments 
sem_CSegments_Nil  =
    (T_CSegments (\ _lhsIinh
                    _lhsInr
                    _lhsInt
                    _lhsIo_case
                    _lhsIo_cata
                    _lhsIo_costcentre
                    _lhsIo_data
                    _lhsIo_linePragmas
                    _lhsIo_newtypes
                    _lhsIo_pretty
                    _lhsIo_rename
                    _lhsIo_sem
                    _lhsIo_sig
                    _lhsIo_splitsems
                    _lhsIo_strictwrap
                    _lhsIo_traces
                    _lhsIo_unbox
                    _lhsIparamMap
                    _lhsIprefix
                    _lhsIsyn ->
                      (let _lhsOisNil :: Bool
                           _lhsOcomments :: ([String])
                           _lhsOsemDom :: ([Decl])
                           _lhsOsemDomUnfoldGath :: (Map (NontermIdent, Int) ([String], Code.Type))
                           _lhsOwrapDecls :: Decls
                           -- "GenerateCode.ag"(line 235, column 10)
                           _lhsOisNil =
                               True
                           -- use rule "GenerateCode.ag"(line 738, column 52)
                           _lhsOcomments =
                               []
                           -- use rule "GenerateCode.ag"(line 592, column 50)
                           _lhsOsemDom =
                               []
                           -- use rule "GenerateCode.ag"(line 611, column 86)
                           _lhsOsemDomUnfoldGath =
                               Map.empty
                           -- use rule "GenerateCode.ag"(line 687, column 52)
                           _lhsOwrapDecls =
                               []
                       in  ( _lhsOcomments,_lhsOisNil,_lhsOsemDom,_lhsOsemDomUnfoldGath,_lhsOwrapDecls))) )
-- CVisit ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         children             : [(Identifier,Type,Bool)]
         con                  : ConstructorIdent
         contextMap           : ContextMap
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         isLast               : Bool
         nextIntra            : Exprs
         nextIntraVars        : Set String
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
      chained attribute:
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         comments             : [String]
         decls                : Decls
         gatherInstVisitNrs   : Map Identifier Int
         intra                : Exprs
         intraVars            : Set String
         semNames             : [String]
   alternatives:
      alternative CVisit:
         child inh            : {Attributes}
         child syn            : {Attributes}
         child vss            : Sequence 
         child intra          : Sequence 
         child ordered        : {Bool}
         visit 0:
            local _tup4       : _
            local higherOrderChildren : _
            local firstOrderChildren : _
            local funcname    : _
            local nextVisitName : _
            local nextVisitDecl : _
            local decls       : _
            local lastExprVars : _
            local _tup5       : _
            local blockFunDecls : _
            local blockFirstFunCall : _
            local costCentreDescr : _
            local addCostCentre : _
            local params      : _
            local semFun      : _
            local tsig        : _
            local semType     : _
            local typeSigs    : _
            local o_case      : _
            local o_splitsems : _
-}
-- cata
sem_CVisit :: CVisit  ->
              T_CVisit 
sem_CVisit (CVisit _inh _syn _vss _intra _ordered )  =
    (sem_CVisit_CVisit _inh _syn (sem_Sequence _vss ) (sem_Sequence _intra ) _ordered )
-- semantic domain
newtype T_CVisit  = T_CVisit ((Set NontermIdent) ->
                              PragmaMap ->
                              ([(Identifier,Type,Bool)]) ->
                              ConstructorIdent ->
                              ContextMap ->
                              (Seq Error) ->
                              Attributes ->
                              (Map Identifier Int) ->
                              Bool ->
                              Exprs ->
                              (Set String) ->
                              Int ->
                              NontermIdent ->
                              Bool ->
                              Bool ->
                              Bool ->
                              (Maybe Bool) ->
                              Bool ->
                              Bool ->
                              Bool ->
                              Bool ->
                              Bool ->
                              Bool ->
                              Bool ->
                              Bool ->
                              Bool ->
                              Bool ->
                              (Map Identifier (NontermIdent, [String])) ->
                              ParamMap ->
                              String ->
                              Attributes ->
                              ([Identifier]) ->
                              (NontermIdent -> Int -> [String] -> Code.Type) ->
                              Bool ->
                              ( Bool,([String]),Decls,(Seq Error),(Map Identifier Int),Exprs,(Set String),([String])))
data Inh_CVisit  = Inh_CVisit {allNts_Inh_CVisit :: !(Set NontermIdent),allPragmas_Inh_CVisit :: !(PragmaMap),children_Inh_CVisit :: !([(Identifier,Type,Bool)]),con_Inh_CVisit :: !(ConstructorIdent),contextMap_Inh_CVisit :: !(ContextMap),errors_Inh_CVisit :: !(Seq Error),inh_Inh_CVisit :: !(Attributes),instVisitNrs_Inh_CVisit :: !(Map Identifier Int),isLast_Inh_CVisit :: !(Bool),nextIntra_Inh_CVisit :: !(Exprs),nextIntraVars_Inh_CVisit :: !(Set String),nr_Inh_CVisit :: !(Int),nt_Inh_CVisit :: !(NontermIdent),o_case_Inh_CVisit :: !(Bool),o_cata_Inh_CVisit :: !(Bool),o_costcentre_Inh_CVisit :: !(Bool),o_data_Inh_CVisit :: !(Maybe Bool),o_linePragmas_Inh_CVisit :: !(Bool),o_newtypes_Inh_CVisit :: !(Bool),o_pretty_Inh_CVisit :: !(Bool),o_rename_Inh_CVisit :: !(Bool),o_sem_Inh_CVisit :: !(Bool),o_sig_Inh_CVisit :: !(Bool),o_splitsems_Inh_CVisit :: !(Bool),o_strictwrap_Inh_CVisit :: !(Bool),o_traces_Inh_CVisit :: !(Bool),o_unbox_Inh_CVisit :: !(Bool),paramInstMap_Inh_CVisit :: !(Map Identifier (NontermIdent, [String])),paramMap_Inh_CVisit :: !(ParamMap),prefix_Inh_CVisit :: !(String),syn_Inh_CVisit :: !(Attributes),terminals_Inh_CVisit :: !([Identifier]),unfoldSemDom_Inh_CVisit :: !(NontermIdent -> Int -> [String] -> Code.Type),with_sig_Inh_CVisit :: !(Bool)}
data Syn_CVisit  = Syn_CVisit {allTpsFound_Syn_CVisit :: !(Bool),comments_Syn_CVisit :: !([String]),decls_Syn_CVisit :: !(Decls),errors_Syn_CVisit :: !(Seq Error),gatherInstVisitNrs_Syn_CVisit :: !(Map Identifier Int),intra_Syn_CVisit :: !(Exprs),intraVars_Syn_CVisit :: !(Set String),semNames_Syn_CVisit :: !([String])}
wrap_CVisit :: T_CVisit  ->
               Inh_CVisit  ->
               Syn_CVisit 
wrap_CVisit (T_CVisit sem ) (Inh_CVisit _lhsIallNts _lhsIallPragmas _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIerrors _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwith_sig )  =
    (let ( _lhsOallTpsFound,_lhsOcomments,_lhsOdecls,_lhsOerrors,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOsemNames) =
             (sem _lhsIallNts _lhsIallPragmas _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIerrors _lhsIinh _lhsIinstVisitNrs _lhsIisLast _lhsInextIntra _lhsInextIntraVars _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwith_sig )
     in  (Syn_CVisit _lhsOallTpsFound _lhsOcomments _lhsOdecls _lhsOerrors _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOsemNames ))
sem_CVisit_CVisit :: Attributes ->
                     Attributes ->
                     T_Sequence  ->
                     T_Sequence  ->
                     Bool ->
                     T_CVisit 
sem_CVisit_CVisit inh_ syn_ (T_Sequence vss_ ) (T_Sequence intra_ ) ordered_  =
    (T_CVisit (\ _lhsIallNts
                 _lhsIallPragmas
                 _lhsIchildren
                 _lhsIcon
                 _lhsIcontextMap
                 _lhsIerrors
                 _lhsIinh
                 _lhsIinstVisitNrs
                 _lhsIisLast
                 _lhsInextIntra
                 _lhsInextIntraVars
                 _lhsInr
                 _lhsInt
                 _lhsIo_case
                 _lhsIo_cata
                 _lhsIo_costcentre
                 _lhsIo_data
                 _lhsIo_linePragmas
                 _lhsIo_newtypes
                 _lhsIo_pretty
                 _lhsIo_rename
                 _lhsIo_sem
                 _lhsIo_sig
                 _lhsIo_splitsems
                 _lhsIo_strictwrap
                 _lhsIo_traces
                 _lhsIo_unbox
                 _lhsIparamInstMap
                 _lhsIparamMap
                 _lhsIprefix
                 _lhsIsyn
                 _lhsIterminals
                 _lhsIunfoldSemDom
                 _lhsIwith_sig ->
                   (let _lhsOintra :: Exprs
                        _lhsOintraVars :: (Set String)
                        _vssOlastExpr :: Expr
                        _intraOlastExpr :: Expr
                        _lhsOdecls :: Decls
                        _lhsOgatherInstVisitNrs :: (Map Identifier Int)
                        _vssOdeclsAbove :: ([Decl])
                        _intraOdeclsAbove :: ([Decl])
                        _lhsOallTpsFound :: Bool
                        _lhsOerrors :: (Seq Error)
                        _lhsOcomments :: ([String])
                        _vssOwhat :: String
                        _intraOwhat :: String
                        _lhsOsemNames :: ([String])
                        _vssOallNts :: (Set NontermIdent)
                        _vssOchildren :: ([(Identifier,Type,Bool)])
                        _vssOcon :: ConstructorIdent
                        _vssOerrors :: (Seq Error)
                        _vssOhigherOrderChildren :: ([(Identifier,Type,Bool)])
                        _vssOinh :: Attributes
                        _vssOinstVisitNrs :: (Map Identifier Int)
                        _vssOnr :: Int
                        _vssOnt :: NontermIdent
                        _vssOo_case :: Bool
                        _vssOo_cata :: Bool
                        _vssOo_costcentre :: Bool
                        _vssOo_data :: (Maybe Bool)
                        _vssOo_linePragmas :: Bool
                        _vssOo_newtypes :: Bool
                        _vssOo_pretty :: Bool
                        _vssOo_rename :: Bool
                        _vssOo_sem :: Bool
                        _vssOo_sig :: Bool
                        _vssOo_splitsems :: Bool
                        _vssOo_strictwrap :: Bool
                        _vssOo_traces :: Bool
                        _vssOo_unbox :: Bool
                        _vssOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                        _vssOparamMap :: ParamMap
                        _vssOprefix :: String
                        _vssOsyn :: Attributes
                        _vssOterminals :: ([Identifier])
                        _vssOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                        _intraOallNts :: (Set NontermIdent)
                        _intraOchildren :: ([(Identifier,Type,Bool)])
                        _intraOcon :: ConstructorIdent
                        _intraOerrors :: (Seq Error)
                        _intraOhigherOrderChildren :: ([(Identifier,Type,Bool)])
                        _intraOinh :: Attributes
                        _intraOinstVisitNrs :: (Map Identifier Int)
                        _intraOnr :: Int
                        _intraOnt :: NontermIdent
                        _intraOo_case :: Bool
                        _intraOo_cata :: Bool
                        _intraOo_costcentre :: Bool
                        _intraOo_data :: (Maybe Bool)
                        _intraOo_linePragmas :: Bool
                        _intraOo_newtypes :: Bool
                        _intraOo_pretty :: Bool
                        _intraOo_rename :: Bool
                        _intraOo_sem :: Bool
                        _intraOo_sig :: Bool
                        _intraOo_splitsems :: Bool
                        _intraOo_strictwrap :: Bool
                        _intraOo_traces :: Bool
                        _intraOo_unbox :: Bool
                        _intraOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                        _intraOparamMap :: ParamMap
                        _intraOprefix :: String
                        _intraOsyn :: Attributes
                        _intraOterminals :: ([Identifier])
                        _intraOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                        _vssIallTpsFound :: Bool
                        _vssIblockDecls :: DeclBlocks
                        _vssIcomments :: ([String])
                        _vssIdecls :: Decls
                        _vssIdeclsAbove :: ([Decl])
                        _vssIdefinedInsts :: ([Identifier])
                        _vssIerrors :: (Seq Error)
                        _vssIexprs :: Exprs
                        _vssItSigs :: ([Decl])
                        _vssItps :: ([Type])
                        _vssIusedVars :: (Set String)
                        _intraIallTpsFound :: Bool
                        _intraIblockDecls :: DeclBlocks
                        _intraIcomments :: ([String])
                        _intraIdecls :: Decls
                        _intraIdeclsAbove :: ([Decl])
                        _intraIdefinedInsts :: ([Identifier])
                        _intraIerrors :: (Seq Error)
                        _intraIexprs :: Exprs
                        _intraItSigs :: ([Decl])
                        _intraItps :: ([Type])
                        _intraIusedVars :: (Set String)
                        -- "GenerateCode.ag"(line 244, column 13)
                        _lhsOintra =
                            _intraIexprs
                        -- "GenerateCode.ag"(line 245, column 13)
                        _lhsOintraVars =
                            _intraIusedVars
                        -- "GenerateCode.ag"(line 357, column 13)
                        __tup4 =
                            partition (\(_,_,ho) -> ho) _lhsIchildren
                        -- "GenerateCode.ag"(line 357, column 13)
                        (_higherOrderChildren,_) =
                            __tup4
                        -- "GenerateCode.ag"(line 357, column 13)
                        (_,_firstOrderChildren) =
                            __tup4
                        -- "GenerateCode.ag"(line 358, column 13)
                        _funcname =
                            seqSemname _lhsIprefix _lhsInt _lhsIcon _lhsInr
                        -- "GenerateCode.ag"(line 359, column 13)
                        _nextVisitName =
                            if _lhsIisLast then [] else [visitname _lhsIprefix _lhsInt (_lhsInr+1)]
                        -- "GenerateCode.ag"(line 360, column 13)
                        _nextVisitDecl =
                            let  lhs = TupleLhs _nextVisitName
                                 rhs = App fun _lhsInextIntra
                                 fun = seqSemname _lhsIprefix _lhsInt _lhsIcon (_lhsInr+1)
                            in if _lhsIisLast
                               then []
                               else [Decl lhs rhs (Set.fromList _nextVisitName) _lhsInextIntraVars]
                        -- "GenerateCode.ag"(line 366, column 13)
                        _decls =
                            _typeSigs ++ _vssIdecls ++ _nextVisitDecl
                        -- "GenerateCode.ag"(line 367, column 13)
                        _vssOlastExpr =
                            mkTupleExpr _lhsIo_unbox (null $ Map.keys inh_) $ map (SimpleExpr . lhsname False) (Map.keys syn_) ++ map SimpleExpr _nextVisitName
                        -- "GenerateCode.ag"(line 368, column 13)
                        _intraOlastExpr =
                            error "lastExpr: not used here"
                        -- "GenerateCode.ag"(line 369, column 13)
                        _lastExprVars =
                            map (lhsname False) (Map.keys syn_) ++ _nextVisitName
                        -- "GenerateCode.ag"(line 370, column 13)
                        __tup5 =
                            mkPartitionedFunction _funcname     _o_case     _nextVisitDecl     _lastExprVars     _vssIblockDecls
                        -- "GenerateCode.ag"(line 370, column 13)
                        (_blockFunDecls,_) =
                            __tup5
                        -- "GenerateCode.ag"(line 370, column 13)
                        (_,_blockFirstFunCall) =
                            __tup5
                        -- "GenerateCode.ag"(line 372, column 13)
                        _costCentreDescr =
                            "b" ++ ":" ++ show _lhsInt ++ ":" ++ show _lhsIcon ++ ":" ++ show _lhsInr
                        -- "GenerateCode.ag"(line 373, column 13)
                        _addCostCentre =
                            \v -> if _lhsIo_costcentre
                                  then PragmaExpr True False ("SCC \"" ++ _costCentreDescr     ++ "\"") v
                                  else v
                        -- "GenerateCode.ag"(line 377, column 13)
                        _params =
                            map getName $ Map.findWithDefault [] _lhsInt _lhsIparamMap
                        -- "GenerateCode.ag"(line 378, column 13)
                        _semFun =
                            let  lhs = Fun _funcname lhs_args
                                 lhs_args = if _lhsInr == 0 then map field _firstOrderChildren     else _intraIexprs
                                 field (name,NT tp tps,_) = let unwrap | _lhsIo_newtypes = \x -> App (sdtype tp) [x]
                                                                       | otherwise       = id
                                                                addType expr | null tps  = expr
                                                                             | otherwise = TypedExpr expr (_lhsIunfoldSemDom tp 0 tps)
                                                            in unwrap $ addType $ SimpleExpr $ funname name 0
                                 field (name,tp,_)        = let expr = SimpleExpr (funname name 0)
                                                            in if null _params
                                                               then expr
                                                               else TypedExpr expr (idEvalType $ SimpleType (typeToHaskellString (Just _lhsInt) _params     tp))
                                 mbEvalTp | null _params     = const Nothing
                                          | otherwise        = Just . idEvalType
                                 rhs = wrap
                                     . mkLambda [mkLambdaArg (lhsname True nm) (mbEvalTp $ SimpleType $ typeToHaskellString (Just _lhsInt) _params     tp) | (nm,tp) <- Map.assocs inh_]
                                     $ _addCostCentre
                                     $ if ordered_ && _o_splitsems
                                       then _blockFirstFunCall
                                       else mkLet _o_case _decls
                                            . mkTupleExpr _lhsIo_unbox (null $ Map.keys inh_)
                                            $ map (SimpleExpr . lhsname False) (Map.keys syn_) ++ map SimpleExpr _nextVisitName
                                 wrap = if  _lhsIo_newtypes
                                            then \x -> App (typeName _lhsInt _lhsInr) [x]
                                            else id
                            in Decl lhs rhs Set.empty Set.empty
                        -- "GenerateCode.ag"(line 408, column 13)
                        _tsig =
                            TSig _funcname _semType
                        -- "GenerateCode.ag"(line 409, column 13)
                        _semType =
                            let argType (NT tp tps)  rec | tp /= _SELF = typeAppStrs (sdtype tp) tps `Arr` rec
                                                         | tp == _SELF = error "GenerateCode: found an intra-type with type SELF, which should have been prevented by CRule.tps"
                                argType (Haskell tp) rec               = SimpleType tp          `Arr` rec
                                evalTp | null _params     = id
                                       | otherwise        = idEvalType
                            in appContext _lhsIcontextMap _lhsInt $ evalTp $
                               if  _lhsInr == 0
                                   then foldr argType (typeAppStrs (sdtype   _lhsInt        ) _params    ) (map (\(_,t,_) -> t) _firstOrderChildren    )
                                   else foldr argType (typeAppStrs (typeName _lhsInt _lhsInr) _params    ) _intraItps
                        -- "GenerateCode.ag"(line 420, column 13)
                        _lhsOdecls =
                            ( if  _lhsIwith_sig
                              then [_tsig, _semFun]
                              else [_semFun]
                            ) ++
                            ( if ordered_ && _o_splitsems
                              then _blockFunDecls
                              else []
                            )
                        -- "GenerateCode.ag"(line 428, column 13)
                        _typeSigs =
                            if  _lhsIo_sig && not _o_case
                                then  _vssItSigs
                                else  []
                        -- "GenerateCode.ag"(line 431, column 13)
                        _o_case =
                            _lhsIo_case && ordered_ && not (hasPragma _lhsIallPragmas _lhsInt _lhsIcon _NOCASE)
                        -- "GenerateCode.ag"(line 432, column 13)
                        _o_splitsems =
                            ordered_ && _lhsIo_splitsems
                        -- "GenerateCode.ag"(line 454, column 7)
                        _lhsOgatherInstVisitNrs =
                            Map.fromList [(i,_lhsInr) | i <- _vssIdefinedInsts]
                        -- "GenerateCode.ag"(line 465, column 7)
                        _vssOdeclsAbove =
                            []
                        -- "GenerateCode.ag"(line 466, column 7)
                        _intraOdeclsAbove =
                            error "declsAbove: not used here"
                        -- "GenerateCode.ag"(line 712, column 13)
                        _lhsOallTpsFound =
                            _intraIallTpsFound
                        -- "GenerateCode.ag"(line 723, column 13)
                        _lhsOerrors =
                            _intraIerrors
                        -- "GenerateCode.ag"(line 756, column 18)
                        _lhsOcomments =
                            let body = map ind (_vssIcomments ++ _intraIcomments)
                            in if null body
                               then []
                               else ("visit " ++ show _lhsInr ++ ":") : body
                        -- "GenerateCode.ag"(line 760, column 18)
                        _vssOwhat =
                            "local"
                        -- "GenerateCode.ag"(line 761, column 18)
                        _intraOwhat =
                            "intra"
                        -- "GenerateCode.ag"(line 987, column 7)
                        _lhsOsemNames =
                            [_funcname    ]
                        -- copy rule (down)
                        _vssOallNts =
                            _lhsIallNts
                        -- copy rule (down)
                        _vssOchildren =
                            _lhsIchildren
                        -- copy rule (down)
                        _vssOcon =
                            _lhsIcon
                        -- copy rule (down)
                        _vssOerrors =
                            _lhsIerrors
                        -- copy rule (from local)
                        _vssOhigherOrderChildren =
                            _higherOrderChildren
                        -- copy rule (down)
                        _vssOinh =
                            _lhsIinh
                        -- copy rule (down)
                        _vssOinstVisitNrs =
                            _lhsIinstVisitNrs
                        -- copy rule (down)
                        _vssOnr =
                            _lhsInr
                        -- copy rule (down)
                        _vssOnt =
                            _lhsInt
                        -- copy rule (from local)
                        _vssOo_case =
                            _o_case
                        -- copy rule (down)
                        _vssOo_cata =
                            _lhsIo_cata
                        -- copy rule (down)
                        _vssOo_costcentre =
                            _lhsIo_costcentre
                        -- copy rule (down)
                        _vssOo_data =
                            _lhsIo_data
                        -- copy rule (down)
                        _vssOo_linePragmas =
                            _lhsIo_linePragmas
                        -- copy rule (down)
                        _vssOo_newtypes =
                            _lhsIo_newtypes
                        -- copy rule (down)
                        _vssOo_pretty =
                            _lhsIo_pretty
                        -- copy rule (down)
                        _vssOo_rename =
                            _lhsIo_rename
                        -- copy rule (down)
                        _vssOo_sem =
                            _lhsIo_sem
                        -- copy rule (down)
                        _vssOo_sig =
                            _lhsIo_sig
                        -- copy rule (from local)
                        _vssOo_splitsems =
                            _o_splitsems
                        -- copy rule (down)
                        _vssOo_strictwrap =
                            _lhsIo_strictwrap
                        -- copy rule (down)
                        _vssOo_traces =
                            _lhsIo_traces
                        -- copy rule (down)
                        _vssOo_unbox =
                            _lhsIo_unbox
                        -- copy rule (down)
                        _vssOparamInstMap =
                            _lhsIparamInstMap
                        -- copy rule (down)
                        _vssOparamMap =
                            _lhsIparamMap
                        -- copy rule (down)
                        _vssOprefix =
                            _lhsIprefix
                        -- copy rule (down)
                        _vssOsyn =
                            _lhsIsyn
                        -- copy rule (down)
                        _vssOterminals =
                            _lhsIterminals
                        -- copy rule (down)
                        _vssOunfoldSemDom =
                            _lhsIunfoldSemDom
                        -- copy rule (down)
                        _intraOallNts =
                            _lhsIallNts
                        -- copy rule (down)
                        _intraOchildren =
                            _lhsIchildren
                        -- copy rule (down)
                        _intraOcon =
                            _lhsIcon
                        -- copy rule (chain)
                        _intraOerrors =
                            _vssIerrors
                        -- copy rule (from local)
                        _intraOhigherOrderChildren =
                            _higherOrderChildren
                        -- copy rule (down)
                        _intraOinh =
                            _lhsIinh
                        -- copy rule (down)
                        _intraOinstVisitNrs =
                            _lhsIinstVisitNrs
                        -- copy rule (down)
                        _intraOnr =
                            _lhsInr
                        -- copy rule (down)
                        _intraOnt =
                            _lhsInt
                        -- copy rule (from local)
                        _intraOo_case =
                            _o_case
                        -- copy rule (down)
                        _intraOo_cata =
                            _lhsIo_cata
                        -- copy rule (down)
                        _intraOo_costcentre =
                            _lhsIo_costcentre
                        -- copy rule (down)
                        _intraOo_data =
                            _lhsIo_data
                        -- copy rule (down)
                        _intraOo_linePragmas =
                            _lhsIo_linePragmas
                        -- copy rule (down)
                        _intraOo_newtypes =
                            _lhsIo_newtypes
                        -- copy rule (down)
                        _intraOo_pretty =
                            _lhsIo_pretty
                        -- copy rule (down)
                        _intraOo_rename =
                            _lhsIo_rename
                        -- copy rule (down)
                        _intraOo_sem =
                            _lhsIo_sem
                        -- copy rule (down)
                        _intraOo_sig =
                            _lhsIo_sig
                        -- copy rule (from local)
                        _intraOo_splitsems =
                            _o_splitsems
                        -- copy rule (down)
                        _intraOo_strictwrap =
                            _lhsIo_strictwrap
                        -- copy rule (down)
                        _intraOo_traces =
                            _lhsIo_traces
                        -- copy rule (down)
                        _intraOo_unbox =
                            _lhsIo_unbox
                        -- copy rule (down)
                        _intraOparamInstMap =
                            _lhsIparamInstMap
                        -- copy rule (down)
                        _intraOparamMap =
                            _lhsIparamMap
                        -- copy rule (down)
                        _intraOprefix =
                            _lhsIprefix
                        -- copy rule (down)
                        _intraOsyn =
                            _lhsIsyn
                        -- copy rule (down)
                        _intraOterminals =
                            _lhsIterminals
                        -- copy rule (down)
                        _intraOunfoldSemDom =
                            _lhsIunfoldSemDom
                        ( _vssIallTpsFound,_vssIblockDecls,_vssIcomments,_vssIdecls,_vssIdeclsAbove,_vssIdefinedInsts,_vssIerrors,_vssIexprs,_vssItSigs,_vssItps,_vssIusedVars) =
                            (vss_ _vssOallNts _vssOchildren _vssOcon _vssOdeclsAbove _vssOerrors _vssOhigherOrderChildren _vssOinh _vssOinstVisitNrs _vssOlastExpr _vssOnr _vssOnt _vssOo_case _vssOo_cata _vssOo_costcentre _vssOo_data _vssOo_linePragmas _vssOo_newtypes _vssOo_pretty _vssOo_rename _vssOo_sem _vssOo_sig _vssOo_splitsems _vssOo_strictwrap _vssOo_traces _vssOo_unbox _vssOparamInstMap _vssOparamMap _vssOprefix _vssOsyn _vssOterminals _vssOunfoldSemDom _vssOwhat )
                        ( _intraIallTpsFound,_intraIblockDecls,_intraIcomments,_intraIdecls,_intraIdeclsAbove,_intraIdefinedInsts,_intraIerrors,_intraIexprs,_intraItSigs,_intraItps,_intraIusedVars) =
                            (intra_ _intraOallNts _intraOchildren _intraOcon _intraOdeclsAbove _intraOerrors _intraOhigherOrderChildren _intraOinh _intraOinstVisitNrs _intraOlastExpr _intraOnr _intraOnt _intraOo_case _intraOo_cata _intraOo_costcentre _intraOo_data _intraOo_linePragmas _intraOo_newtypes _intraOo_pretty _intraOo_rename _intraOo_sem _intraOo_sig _intraOo_splitsems _intraOo_strictwrap _intraOo_traces _intraOo_unbox _intraOparamInstMap _intraOparamMap _intraOprefix _intraOsyn _intraOterminals _intraOunfoldSemDom _intraOwhat )
                    in  ( _lhsOallTpsFound,_lhsOcomments,_lhsOdecls,_lhsOerrors,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOsemNames))) )
-- CVisits -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         allPragmas           : PragmaMap
         children             : [(Identifier,Type,Bool)]
         con                  : ConstructorIdent
         contextMap           : ContextMap
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         with_sig             : Bool
      chained attribute:
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         comments             : [String]
         decls                : Decls
         gatherInstVisitNrs   : Map Identifier Int
         intra                : Exprs
         intraVars            : Set String
         isNil                : Bool
         semNames             : [String]
   alternatives:
      alternative Cons:
         child hd             : CVisit 
         child tl             : CVisits 
      alternative Nil:
-}
-- cata
sem_CVisits :: CVisits  ->
               T_CVisits 
sem_CVisits list  =
    (Prelude.foldr sem_CVisits_Cons sem_CVisits_Nil (Prelude.map sem_CVisit list) )
-- semantic domain
newtype T_CVisits  = T_CVisits ((Set NontermIdent) ->
                                PragmaMap ->
                                ([(Identifier,Type,Bool)]) ->
                                ConstructorIdent ->
                                ContextMap ->
                                (Seq Error) ->
                                Attributes ->
                                (Map Identifier Int) ->
                                Int ->
                                NontermIdent ->
                                Bool ->
                                Bool ->
                                Bool ->
                                (Maybe Bool) ->
                                Bool ->
                                Bool ->
                                Bool ->
                                Bool ->
                                Bool ->
                                Bool ->
                                Bool ->
                                Bool ->
                                Bool ->
                                Bool ->
                                (Map Identifier (NontermIdent, [String])) ->
                                ParamMap ->
                                String ->
                                Attributes ->
                                ([Identifier]) ->
                                (NontermIdent -> Int -> [String] -> Code.Type) ->
                                Bool ->
                                ( Bool,([String]),Decls,(Seq Error),(Map Identifier Int),Exprs,(Set String),Bool,([String])))
data Inh_CVisits  = Inh_CVisits {allNts_Inh_CVisits :: !(Set NontermIdent),allPragmas_Inh_CVisits :: !(PragmaMap),children_Inh_CVisits :: !([(Identifier,Type,Bool)]),con_Inh_CVisits :: !(ConstructorIdent),contextMap_Inh_CVisits :: !(ContextMap),errors_Inh_CVisits :: !(Seq Error),inh_Inh_CVisits :: !(Attributes),instVisitNrs_Inh_CVisits :: !(Map Identifier Int),nr_Inh_CVisits :: !(Int),nt_Inh_CVisits :: !(NontermIdent),o_case_Inh_CVisits :: !(Bool),o_cata_Inh_CVisits :: !(Bool),o_costcentre_Inh_CVisits :: !(Bool),o_data_Inh_CVisits :: !(Maybe Bool),o_linePragmas_Inh_CVisits :: !(Bool),o_newtypes_Inh_CVisits :: !(Bool),o_pretty_Inh_CVisits :: !(Bool),o_rename_Inh_CVisits :: !(Bool),o_sem_Inh_CVisits :: !(Bool),o_sig_Inh_CVisits :: !(Bool),o_splitsems_Inh_CVisits :: !(Bool),o_strictwrap_Inh_CVisits :: !(Bool),o_traces_Inh_CVisits :: !(Bool),o_unbox_Inh_CVisits :: !(Bool),paramInstMap_Inh_CVisits :: !(Map Identifier (NontermIdent, [String])),paramMap_Inh_CVisits :: !(ParamMap),prefix_Inh_CVisits :: !(String),syn_Inh_CVisits :: !(Attributes),terminals_Inh_CVisits :: !([Identifier]),unfoldSemDom_Inh_CVisits :: !(NontermIdent -> Int -> [String] -> Code.Type),with_sig_Inh_CVisits :: !(Bool)}
data Syn_CVisits  = Syn_CVisits {allTpsFound_Syn_CVisits :: !(Bool),comments_Syn_CVisits :: !([String]),decls_Syn_CVisits :: !(Decls),errors_Syn_CVisits :: !(Seq Error),gatherInstVisitNrs_Syn_CVisits :: !(Map Identifier Int),intra_Syn_CVisits :: !(Exprs),intraVars_Syn_CVisits :: !(Set String),isNil_Syn_CVisits :: !(Bool),semNames_Syn_CVisits :: !([String])}
wrap_CVisits :: T_CVisits  ->
                Inh_CVisits  ->
                Syn_CVisits 
wrap_CVisits (T_CVisits sem ) (Inh_CVisits _lhsIallNts _lhsIallPragmas _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIerrors _lhsIinh _lhsIinstVisitNrs _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwith_sig )  =
    (let ( _lhsOallTpsFound,_lhsOcomments,_lhsOdecls,_lhsOerrors,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOisNil,_lhsOsemNames) =
             (sem _lhsIallNts _lhsIallPragmas _lhsIchildren _lhsIcon _lhsIcontextMap _lhsIerrors _lhsIinh _lhsIinstVisitNrs _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwith_sig )
     in  (Syn_CVisits _lhsOallTpsFound _lhsOcomments _lhsOdecls _lhsOerrors _lhsOgatherInstVisitNrs _lhsOintra _lhsOintraVars _lhsOisNil _lhsOsemNames ))
sem_CVisits_Cons :: T_CVisit  ->
                    T_CVisits  ->
                    T_CVisits 
sem_CVisits_Cons (T_CVisit hd_ ) (T_CVisits tl_ )  =
    (T_CVisits (\ _lhsIallNts
                  _lhsIallPragmas
                  _lhsIchildren
                  _lhsIcon
                  _lhsIcontextMap
                  _lhsIerrors
                  _lhsIinh
                  _lhsIinstVisitNrs
                  _lhsInr
                  _lhsInt
                  _lhsIo_case
                  _lhsIo_cata
                  _lhsIo_costcentre
                  _lhsIo_data
                  _lhsIo_linePragmas
                  _lhsIo_newtypes
                  _lhsIo_pretty
                  _lhsIo_rename
                  _lhsIo_sem
                  _lhsIo_sig
                  _lhsIo_splitsems
                  _lhsIo_strictwrap
                  _lhsIo_traces
                  _lhsIo_unbox
                  _lhsIparamInstMap
                  _lhsIparamMap
                  _lhsIprefix
                  _lhsIsyn
                  _lhsIterminals
                  _lhsIunfoldSemDom
                  _lhsIwith_sig ->
                    (let _tlOnr :: Int
                         _lhsOisNil :: Bool
                         _hdOisLast :: Bool
                         _hdOnextIntra :: Exprs
                         _hdOnextIntraVars :: (Set String)
                         _lhsOintra :: Exprs
                         _lhsOintraVars :: (Set String)
                         _lhsOallTpsFound :: Bool
                         _lhsOcomments :: ([String])
                         _lhsOdecls :: Decls
                         _lhsOerrors :: (Seq Error)
                         _lhsOgatherInstVisitNrs :: (Map Identifier Int)
                         _lhsOsemNames :: ([String])
                         _hdOallNts :: (Set NontermIdent)
                         _hdOallPragmas :: PragmaMap
                         _hdOchildren :: ([(Identifier,Type,Bool)])
                         _hdOcon :: ConstructorIdent
                         _hdOcontextMap :: ContextMap
                         _hdOerrors :: (Seq Error)
                         _hdOinh :: Attributes
                         _hdOinstVisitNrs :: (Map Identifier Int)
                         _hdOnr :: Int
                         _hdOnt :: NontermIdent
                         _hdOo_case :: Bool
                         _hdOo_cata :: Bool
                         _hdOo_costcentre :: Bool
                         _hdOo_data :: (Maybe Bool)
                         _hdOo_linePragmas :: Bool
                         _hdOo_newtypes :: Bool
                         _hdOo_pretty :: Bool
                         _hdOo_rename :: Bool
                         _hdOo_sem :: Bool
                         _hdOo_sig :: Bool
                         _hdOo_splitsems :: Bool
                         _hdOo_strictwrap :: Bool
                         _hdOo_traces :: Bool
                         _hdOo_unbox :: Bool
                         _hdOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                         _hdOparamMap :: ParamMap
                         _hdOprefix :: String
                         _hdOsyn :: Attributes
                         _hdOterminals :: ([Identifier])
                         _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                         _hdOwith_sig :: Bool
                         _tlOallNts :: (Set NontermIdent)
                         _tlOallPragmas :: PragmaMap
                         _tlOchildren :: ([(Identifier,Type,Bool)])
                         _tlOcon :: ConstructorIdent
                         _tlOcontextMap :: ContextMap
                         _tlOerrors :: (Seq Error)
                         _tlOinh :: Attributes
                         _tlOinstVisitNrs :: (Map Identifier Int)
                         _tlOnt :: NontermIdent
                         _tlOo_case :: Bool
                         _tlOo_cata :: Bool
                         _tlOo_costcentre :: Bool
                         _tlOo_data :: (Maybe Bool)
                         _tlOo_linePragmas :: Bool
                         _tlOo_newtypes :: Bool
                         _tlOo_pretty :: Bool
                         _tlOo_rename :: Bool
                         _tlOo_sem :: Bool
                         _tlOo_sig :: Bool
                         _tlOo_splitsems :: Bool
                         _tlOo_strictwrap :: Bool
                         _tlOo_traces :: Bool
                         _tlOo_unbox :: Bool
                         _tlOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                         _tlOparamMap :: ParamMap
                         _tlOprefix :: String
                         _tlOsyn :: Attributes
                         _tlOterminals :: ([Identifier])
                         _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                         _tlOwith_sig :: Bool
                         _hdIallTpsFound :: Bool
                         _hdIcomments :: ([String])
                         _hdIdecls :: Decls
                         _hdIerrors :: (Seq Error)
                         _hdIgatherInstVisitNrs :: (Map Identifier Int)
                         _hdIintra :: Exprs
                         _hdIintraVars :: (Set String)
                         _hdIsemNames :: ([String])
                         _tlIallTpsFound :: Bool
                         _tlIcomments :: ([String])
                         _tlIdecls :: Decls
                         _tlIerrors :: (Seq Error)
                         _tlIgatherInstVisitNrs :: (Map Identifier Int)
                         _tlIintra :: Exprs
                         _tlIintraVars :: (Set String)
                         _tlIisNil :: Bool
                         _tlIsemNames :: ([String])
                         -- "GenerateCode.ag"(line 216, column 11)
                         _tlOnr =
                             _lhsInr + 1
                         -- "GenerateCode.ag"(line 229, column 12)
                         _lhsOisNil =
                             False
                         -- "GenerateCode.ag"(line 230, column 12)
                         _hdOisLast =
                             _tlIisNil
                         -- "GenerateCode.ag"(line 247, column 12)
                         _hdOnextIntra =
                             _tlIintra
                         -- "GenerateCode.ag"(line 248, column 12)
                         _hdOnextIntraVars =
                             _tlIintraVars
                         -- "GenerateCode.ag"(line 249, column 12)
                         _lhsOintra =
                             _hdIintra
                         -- "GenerateCode.ag"(line 250, column 12)
                         _lhsOintraVars =
                             _hdIintraVars
                         -- use rule "GenerateCode.ag"(line 706, column 39)
                         _lhsOallTpsFound =
                             _hdIallTpsFound && _tlIallTpsFound
                         -- use rule "GenerateCode.ag"(line 738, column 52)
                         _lhsOcomments =
                             _hdIcomments ++ _tlIcomments
                         -- use rule "GenerateCode.ag"(line 355, column 33)
                         _lhsOdecls =
                             _hdIdecls ++ _tlIdecls
                         -- use rule "GenerateCode.ag"(line 719, column 32)
                         _lhsOerrors =
                             _hdIerrors Seq.>< _tlIerrors
                         -- use rule "GenerateCode.ag"(line 447, column 44)
                         _lhsOgatherInstVisitNrs =
                             _hdIgatherInstVisitNrs `Map.union` _tlIgatherInstVisitNrs
                         -- use rule "GenerateCode.ag"(line 977, column 61)
                         _lhsOsemNames =
                             _hdIsemNames ++ _tlIsemNames
                         -- copy rule (down)
                         _hdOallNts =
                             _lhsIallNts
                         -- copy rule (down)
                         _hdOallPragmas =
                             _lhsIallPragmas
                         -- copy rule (down)
                         _hdOchildren =
                             _lhsIchildren
                         -- copy rule (down)
                         _hdOcon =
                             _lhsIcon
                         -- copy rule (down)
                         _hdOcontextMap =
                             _lhsIcontextMap
                         -- copy rule (down)
                         _hdOerrors =
                             _lhsIerrors
                         -- copy rule (down)
                         _hdOinh =
                             _lhsIinh
                         -- copy rule (down)
                         _hdOinstVisitNrs =
                             _lhsIinstVisitNrs
                         -- copy rule (down)
                         _hdOnr =
                             _lhsInr
                         -- copy rule (down)
                         _hdOnt =
                             _lhsInt
                         -- copy rule (down)
                         _hdOo_case =
                             _lhsIo_case
                         -- copy rule (down)
                         _hdOo_cata =
                             _lhsIo_cata
                         -- copy rule (down)
                         _hdOo_costcentre =
                             _lhsIo_costcentre
                         -- copy rule (down)
                         _hdOo_data =
                             _lhsIo_data
                         -- copy rule (down)
                         _hdOo_linePragmas =
                             _lhsIo_linePragmas
                         -- copy rule (down)
                         _hdOo_newtypes =
                             _lhsIo_newtypes
                         -- copy rule (down)
                         _hdOo_pretty =
                             _lhsIo_pretty
                         -- copy rule (down)
                         _hdOo_rename =
                             _lhsIo_rename
                         -- copy rule (down)
                         _hdOo_sem =
                             _lhsIo_sem
                         -- copy rule (down)
                         _hdOo_sig =
                             _lhsIo_sig
                         -- copy rule (down)
                         _hdOo_splitsems =
                             _lhsIo_splitsems
                         -- copy rule (down)
                         _hdOo_strictwrap =
                             _lhsIo_strictwrap
                         -- copy rule (down)
                         _hdOo_traces =
                             _lhsIo_traces
                         -- copy rule (down)
                         _hdOo_unbox =
                             _lhsIo_unbox
                         -- copy rule (down)
                         _hdOparamInstMap =
                             _lhsIparamInstMap
                         -- copy rule (down)
                         _hdOparamMap =
                             _lhsIparamMap
                         -- copy rule (down)
                         _hdOprefix =
                             _lhsIprefix
                         -- copy rule (down)
                         _hdOsyn =
                             _lhsIsyn
                         -- copy rule (down)
                         _hdOterminals =
                             _lhsIterminals
                         -- copy rule (down)
                         _hdOunfoldSemDom =
                             _lhsIunfoldSemDom
                         -- copy rule (down)
                         _hdOwith_sig =
                             _lhsIwith_sig
                         -- copy rule (down)
                         _tlOallNts =
                             _lhsIallNts
                         -- copy rule (down)
                         _tlOallPragmas =
                             _lhsIallPragmas
                         -- copy rule (down)
                         _tlOchildren =
                             _lhsIchildren
                         -- copy rule (down)
                         _tlOcon =
                             _lhsIcon
                         -- copy rule (down)
                         _tlOcontextMap =
                             _lhsIcontextMap
                         -- copy rule (chain)
                         _tlOerrors =
                             _hdIerrors
                         -- copy rule (down)
                         _tlOinh =
                             _lhsIinh
                         -- copy rule (down)
                         _tlOinstVisitNrs =
                             _lhsIinstVisitNrs
                         -- copy rule (down)
                         _tlOnt =
                             _lhsInt
                         -- copy rule (down)
                         _tlOo_case =
                             _lhsIo_case
                         -- copy rule (down)
                         _tlOo_cata =
                             _lhsIo_cata
                         -- copy rule (down)
                         _tlOo_costcentre =
                             _lhsIo_costcentre
                         -- copy rule (down)
                         _tlOo_data =
                             _lhsIo_data
                         -- copy rule (down)
                         _tlOo_linePragmas =
                             _lhsIo_linePragmas
                         -- copy rule (down)
                         _tlOo_newtypes =
                             _lhsIo_newtypes
                         -- copy rule (down)
                         _tlOo_pretty =
                             _lhsIo_pretty
                         -- copy rule (down)
                         _tlOo_rename =
                             _lhsIo_rename
                         -- copy rule (down)
                         _tlOo_sem =
                             _lhsIo_sem
                         -- copy rule (down)
                         _tlOo_sig =
                             _lhsIo_sig
                         -- copy rule (down)
                         _tlOo_splitsems =
                             _lhsIo_splitsems
                         -- copy rule (down)
                         _tlOo_strictwrap =
                             _lhsIo_strictwrap
                         -- copy rule (down)
                         _tlOo_traces =
                             _lhsIo_traces
                         -- copy rule (down)
                         _tlOo_unbox =
                             _lhsIo_unbox
                         -- copy rule (down)
                         _tlOparamInstMap =
                             _lhsIparamInstMap
                         -- copy rule (down)
                         _tlOparamMap =
                             _lhsIparamMap
                         -- copy rule (down)
                         _tlOprefix =
                             _lhsIprefix
                         -- copy rule (down)
                         _tlOsyn =
                             _lhsIsyn
                         -- copy rule (down)
                         _tlOterminals =
                             _lhsIterminals
                         -- copy rule (down)
                         _tlOunfoldSemDom =
                             _lhsIunfoldSemDom
                         -- copy rule (down)
                         _tlOwith_sig =
                             _lhsIwith_sig
                         ( _hdIallTpsFound,_hdIcomments,_hdIdecls,_hdIerrors,_hdIgatherInstVisitNrs,_hdIintra,_hdIintraVars,_hdIsemNames) =
                             (hd_ _hdOallNts _hdOallPragmas _hdOchildren _hdOcon _hdOcontextMap _hdOerrors _hdOinh _hdOinstVisitNrs _hdOisLast _hdOnextIntra _hdOnextIntraVars _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOparamInstMap _hdOparamMap _hdOprefix _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOwith_sig )
                         ( _tlIallTpsFound,_tlIcomments,_tlIdecls,_tlIerrors,_tlIgatherInstVisitNrs,_tlIintra,_tlIintraVars,_tlIisNil,_tlIsemNames) =
                             (tl_ _tlOallNts _tlOallPragmas _tlOchildren _tlOcon _tlOcontextMap _tlOerrors _tlOinh _tlOinstVisitNrs _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOparamInstMap _tlOparamMap _tlOprefix _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOwith_sig )
                     in  ( _lhsOallTpsFound,_lhsOcomments,_lhsOdecls,_lhsOerrors,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOisNil,_lhsOsemNames))) )
sem_CVisits_Nil :: T_CVisits 
sem_CVisits_Nil  =
    (T_CVisits (\ _lhsIallNts
                  _lhsIallPragmas
                  _lhsIchildren
                  _lhsIcon
                  _lhsIcontextMap
                  _lhsIerrors
                  _lhsIinh
                  _lhsIinstVisitNrs
                  _lhsInr
                  _lhsInt
                  _lhsIo_case
                  _lhsIo_cata
                  _lhsIo_costcentre
                  _lhsIo_data
                  _lhsIo_linePragmas
                  _lhsIo_newtypes
                  _lhsIo_pretty
                  _lhsIo_rename
                  _lhsIo_sem
                  _lhsIo_sig
                  _lhsIo_splitsems
                  _lhsIo_strictwrap
                  _lhsIo_traces
                  _lhsIo_unbox
                  _lhsIparamInstMap
                  _lhsIparamMap
                  _lhsIprefix
                  _lhsIsyn
                  _lhsIterminals
                  _lhsIunfoldSemDom
                  _lhsIwith_sig ->
                    (let _lhsOisNil :: Bool
                         _lhsOintra :: Exprs
                         _lhsOintraVars :: (Set String)
                         _lhsOallTpsFound :: Bool
                         _lhsOcomments :: ([String])
                         _lhsOdecls :: Decls
                         _lhsOerrors :: (Seq Error)
                         _lhsOgatherInstVisitNrs :: (Map Identifier Int)
                         _lhsOsemNames :: ([String])
                         -- "GenerateCode.ag"(line 231, column 10)
                         _lhsOisNil =
                             True
                         -- "GenerateCode.ag"(line 251, column 10)
                         _lhsOintra =
                             []
                         -- "GenerateCode.ag"(line 252, column 10)
                         _lhsOintraVars =
                             Set.empty
                         -- use rule "GenerateCode.ag"(line 706, column 39)
                         _lhsOallTpsFound =
                             True
                         -- use rule "GenerateCode.ag"(line 738, column 52)
                         _lhsOcomments =
                             []
                         -- use rule "GenerateCode.ag"(line 355, column 33)
                         _lhsOdecls =
                             []
                         -- use rule "GenerateCode.ag"(line 719, column 32)
                         _lhsOerrors =
                             Seq.empty
                         -- use rule "GenerateCode.ag"(line 447, column 44)
                         _lhsOgatherInstVisitNrs =
                             Map.empty
                         -- use rule "GenerateCode.ag"(line 977, column 61)
                         _lhsOsemNames =
                             []
                     in  ( _lhsOallTpsFound,_lhsOcomments,_lhsOdecls,_lhsOerrors,_lhsOgatherInstVisitNrs,_lhsOintra,_lhsOintraVars,_lhsOisNil,_lhsOsemNames))) )
-- DeclBlocks --------------------------------------------------
{-
   visit 0:
      inherited attributes:
         blockNr              : Int
         lastExprVars         : [String]
         nextVisitDecls       : [Decl]
         optCase              : Bool
         prefix               : String
      synthesized attributes:
         callExpr             : Expr
         decls                : [Decl]
         freeVars             : [String]
   alternatives:
      alternative DeclBlock:
         child defs           : {[Decl]}
         child visit          : {Decl}
         child next           : DeclBlocks 
         visit 0:
            local lambdaName  : _
            local pragmaDecl  : _
            local freeVars    : _
            local decl        : _
      alternative DeclTerminator:
         child defs           : {[Decl]}
         child result         : {Expr}
         visit 0:
            local lambdaName  : _
            local pragmaDecl  : _
            local freeVars    : _
-}
-- cata
sem_DeclBlocks :: DeclBlocks  ->
                  T_DeclBlocks 
sem_DeclBlocks (DeclBlock _defs _visit _next )  =
    (sem_DeclBlocks_DeclBlock _defs _visit (sem_DeclBlocks _next ) )
sem_DeclBlocks (DeclTerminator _defs _result )  =
    (sem_DeclBlocks_DeclTerminator _defs _result )
-- semantic domain
newtype T_DeclBlocks  = T_DeclBlocks (Int ->
                                      ([String]) ->
                                      ([Decl]) ->
                                      Bool ->
                                      String ->
                                      ( Expr,([Decl]),([String])))
data Inh_DeclBlocks  = Inh_DeclBlocks {blockNr_Inh_DeclBlocks :: !(Int),lastExprVars_Inh_DeclBlocks :: !([String]),nextVisitDecls_Inh_DeclBlocks :: !([Decl]),optCase_Inh_DeclBlocks :: !(Bool),prefix_Inh_DeclBlocks :: !(String)}
data Syn_DeclBlocks  = Syn_DeclBlocks {callExpr_Syn_DeclBlocks :: !(Expr),decls_Syn_DeclBlocks :: !([Decl]),freeVars_Syn_DeclBlocks :: !([String])}
wrap_DeclBlocks :: T_DeclBlocks  ->
                   Inh_DeclBlocks  ->
                   Syn_DeclBlocks 
wrap_DeclBlocks (T_DeclBlocks sem ) (Inh_DeclBlocks _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix )  =
    (let ( _lhsOcallExpr,_lhsOdecls,_lhsOfreeVars) =
             (sem _lhsIblockNr _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix )
     in  (Syn_DeclBlocks _lhsOcallExpr _lhsOdecls _lhsOfreeVars ))
sem_DeclBlocks_DeclBlock :: ([Decl]) ->
                            Decl ->
                            T_DeclBlocks  ->
                            T_DeclBlocks 
sem_DeclBlocks_DeclBlock defs_ visit_ (T_DeclBlocks next_ )  =
    (T_DeclBlocks (\ _lhsIblockNr
                     _lhsIlastExprVars
                     _lhsInextVisitDecls
                     _lhsIoptCase
                     _lhsIprefix ->
                       (let _nextOblockNr :: Int
                            _lhsOcallExpr :: Expr
                            _lhsOdecls :: ([Decl])
                            _lhsOfreeVars :: ([String])
                            _nextOlastExprVars :: ([String])
                            _nextOnextVisitDecls :: ([Decl])
                            _nextOoptCase :: Bool
                            _nextOprefix :: String
                            _nextIcallExpr :: Expr
                            _nextIdecls :: ([Decl])
                            _nextIfreeVars :: ([String])
                            -- "GenerateCode.ag"(line 544, column 7)
                            _nextOblockNr =
                                _lhsIblockNr + 1
                            -- "GenerateCode.ag"(line 549, column 7)
                            _lambdaName =
                                _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                            -- "GenerateCode.ag"(line 550, column 7)
                            _pragmaDecl =
                                PragmaDecl ("NOINLINE " ++ _lambdaName    )
                            -- "GenerateCode.ag"(line 551, column 7)
                            _lhsOcallExpr =
                                App _lambdaName     (map SimpleExpr _freeVars    )
                            -- "GenerateCode.ag"(line 555, column 7)
                            _freeVars =
                                freevars _nextIfreeVars (visit_ : defs_)
                            -- "GenerateCode.ag"(line 562, column 7)
                            _decl =
                                mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ [visit_]) _nextIcallExpr
                            -- "GenerateCode.ag"(line 563, column 7)
                            _lhsOdecls =
                                (if _lhsIblockNr > 1 then [_pragmaDecl    ] else []) ++ [_decl    ] ++ _nextIdecls
                            -- copy rule (from local)
                            _lhsOfreeVars =
                                _freeVars
                            -- copy rule (down)
                            _nextOlastExprVars =
                                _lhsIlastExprVars
                            -- copy rule (down)
                            _nextOnextVisitDecls =
                                _lhsInextVisitDecls
                            -- copy rule (down)
                            _nextOoptCase =
                                _lhsIoptCase
                            -- copy rule (down)
                            _nextOprefix =
                                _lhsIprefix
                            ( _nextIcallExpr,_nextIdecls,_nextIfreeVars) =
                                (next_ _nextOblockNr _nextOlastExprVars _nextOnextVisitDecls _nextOoptCase _nextOprefix )
                        in  ( _lhsOcallExpr,_lhsOdecls,_lhsOfreeVars))) )
sem_DeclBlocks_DeclTerminator :: ([Decl]) ->
                                 Expr ->
                                 T_DeclBlocks 
sem_DeclBlocks_DeclTerminator defs_ result_  =
    (T_DeclBlocks (\ _lhsIblockNr
                     _lhsIlastExprVars
                     _lhsInextVisitDecls
                     _lhsIoptCase
                     _lhsIprefix ->
                       (let _lhsOcallExpr :: Expr
                            _lhsOdecls :: ([Decl])
                            _lhsOfreeVars :: ([String])
                            -- "GenerateCode.ag"(line 549, column 7)
                            _lambdaName =
                                _lhsIprefix ++ "_block" ++ show _lhsIblockNr
                            -- "GenerateCode.ag"(line 550, column 7)
                            _pragmaDecl =
                                PragmaDecl ("NOINLINE " ++ _lambdaName    )
                            -- "GenerateCode.ag"(line 551, column 7)
                            _lhsOcallExpr =
                                App _lambdaName     (map SimpleExpr _freeVars    )
                            -- "GenerateCode.ag"(line 553, column 7)
                            _freeVars =
                                freevars _lhsIlastExprVars (defs_ ++ _lhsInextVisitDecls)
                            -- "GenerateCode.ag"(line 560, column 7)
                            _lhsOdecls =
                                [ mkBlockLambda _lhsIoptCase _lambdaName     _freeVars     (defs_ ++ _lhsInextVisitDecls) result_ ]
                            -- copy rule (from local)
                            _lhsOfreeVars =
                                _freeVars
                        in  ( _lhsOcallExpr,_lhsOdecls,_lhsOfreeVars))) )
-- DeclBlocksRoot ----------------------------------------------
{-
   visit 0:
      inherited attributes:
         lastExprVars         : [String]
         nextVisitDecls       : [Decl]
         optCase              : Bool
         prefix               : String
      synthesized attributes:
         firstCall            : Expr
         lambdas              : [Decl]
   alternatives:
      alternative DeclBlocksRoot:
         child blocks         : DeclBlocks 
-}
-- cata
sem_DeclBlocksRoot :: DeclBlocksRoot  ->
                      T_DeclBlocksRoot 
sem_DeclBlocksRoot (DeclBlocksRoot _blocks )  =
    (sem_DeclBlocksRoot_DeclBlocksRoot (sem_DeclBlocks _blocks ) )
-- semantic domain
newtype T_DeclBlocksRoot  = T_DeclBlocksRoot (([String]) ->
                                              ([Decl]) ->
                                              Bool ->
                                              String ->
                                              ( Expr,([Decl])))
data Inh_DeclBlocksRoot  = Inh_DeclBlocksRoot {lastExprVars_Inh_DeclBlocksRoot :: !([String]),nextVisitDecls_Inh_DeclBlocksRoot :: !([Decl]),optCase_Inh_DeclBlocksRoot :: !(Bool),prefix_Inh_DeclBlocksRoot :: !(String)}
data Syn_DeclBlocksRoot  = Syn_DeclBlocksRoot {firstCall_Syn_DeclBlocksRoot :: !(Expr),lambdas_Syn_DeclBlocksRoot :: !([Decl])}
wrap_DeclBlocksRoot :: T_DeclBlocksRoot  ->
                       Inh_DeclBlocksRoot  ->
                       Syn_DeclBlocksRoot 
wrap_DeclBlocksRoot (T_DeclBlocksRoot sem ) (Inh_DeclBlocksRoot _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix )  =
    (let ( _lhsOfirstCall,_lhsOlambdas) =
             (sem _lhsIlastExprVars _lhsInextVisitDecls _lhsIoptCase _lhsIprefix )
     in  (Syn_DeclBlocksRoot _lhsOfirstCall _lhsOlambdas ))
sem_DeclBlocksRoot_DeclBlocksRoot :: T_DeclBlocks  ->
                                     T_DeclBlocksRoot 
sem_DeclBlocksRoot_DeclBlocksRoot (T_DeclBlocks blocks_ )  =
    (T_DeclBlocksRoot (\ _lhsIlastExprVars
                         _lhsInextVisitDecls
                         _lhsIoptCase
                         _lhsIprefix ->
                           (let _lhsOlambdas :: ([Decl])
                                _lhsOfirstCall :: Expr
                                _blocksOblockNr :: Int
                                _blocksOlastExprVars :: ([String])
                                _blocksOnextVisitDecls :: ([Decl])
                                _blocksOoptCase :: Bool
                                _blocksOprefix :: String
                                _blocksIcallExpr :: Expr
                                _blocksIdecls :: ([Decl])
                                _blocksIfreeVars :: ([String])
                                -- "GenerateCode.ag"(line 535, column 7)
                                _lhsOlambdas =
                                    _blocksIdecls
                                -- "GenerateCode.ag"(line 536, column 7)
                                _lhsOfirstCall =
                                    _blocksIcallExpr
                                -- "GenerateCode.ag"(line 541, column 7)
                                _blocksOblockNr =
                                    1
                                -- copy rule (down)
                                _blocksOlastExprVars =
                                    _lhsIlastExprVars
                                -- copy rule (down)
                                _blocksOnextVisitDecls =
                                    _lhsInextVisitDecls
                                -- copy rule (down)
                                _blocksOoptCase =
                                    _lhsIoptCase
                                -- copy rule (down)
                                _blocksOprefix =
                                    _lhsIprefix
                                ( _blocksIcallExpr,_blocksIdecls,_blocksIfreeVars) =
                                    (blocks_ _blocksOblockNr _blocksOlastExprVars _blocksOnextVisitDecls _blocksOoptCase _blocksOprefix )
                            in  ( _lhsOfirstCall,_lhsOlambdas))) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         definedInsts         : [Identifier]
         patternAttributes    : [(Identifier, Identifier)]
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
newtype T_Pattern  = T_Pattern (( Pattern,([Identifier]),([(Identifier, Identifier)])))
data Inh_Pattern  = Inh_Pattern {}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),definedInsts_Syn_Pattern :: !([Identifier]),patternAttributes_Syn_Pattern :: !([(Identifier, Identifier)])}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern (T_Pattern sem ) (Inh_Pattern )  =
    (let ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes) =
             (sem )
     in  (Syn_Pattern _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias field_ attr_ (T_Pattern pat_ ) (T_Patterns parts_ )  =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatternAttributes :: ([(Identifier, Identifier)])
                    _partsIcopy :: Patterns
                    _partsIdefinedInsts :: ([Identifier])
                    _partsIpatternAttributes :: ([(Identifier, Identifier)])
                    -- "GenerateCode.ag"(line 197, column 11)
                    _lhsOdefinedInsts =
                        (if field_ == _INST then [attr_] else []) ++ _patIdefinedInsts
                    -- "GenerateCode.ag"(line 205, column 7)
                    _lhsOpatternAttributes =
                        (field_,attr_) : (_patIpatternAttributes ++ _partsIpatternAttributes)
                    -- self rule
                    _copy =
                        Alias field_ attr_ _patIcopy _partsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patIcopy,_patIdefinedInsts,_patIpatternAttributes) =
                        (pat_ )
                    ( _partsIcopy,_partsIdefinedInsts,_partsIpatternAttributes) =
                        (parts_ )
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr name_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatternAttributes :: ([(Identifier, Identifier)])
                    -- use rule "GenerateCode.ag"(line 195, column 55)
                    _lhsOdefinedInsts =
                        _patsIdefinedInsts
                    -- use rule "GenerateCode.ag"(line 202, column 47)
                    _lhsOpatternAttributes =
                        _patsIpatternAttributes
                    -- self rule
                    _copy =
                        Constr name_ _patsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patsIcopy,_patsIdefinedInsts,_patsIpatternAttributes) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable (T_Pattern pat_ )  =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patIcopy :: Pattern
                    _patIdefinedInsts :: ([Identifier])
                    _patIpatternAttributes :: ([(Identifier, Identifier)])
                    -- use rule "GenerateCode.ag"(line 195, column 55)
                    _lhsOdefinedInsts =
                        _patIdefinedInsts
                    -- use rule "GenerateCode.ag"(line 202, column 47)
                    _lhsOpatternAttributes =
                        _patIpatternAttributes
                    -- self rule
                    _copy =
                        Irrefutable _patIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patIcopy,_patIdefinedInsts,_patIpatternAttributes) =
                        (pat_ )
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product pos_ (T_Patterns pats_ )  =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    _patsIcopy :: Patterns
                    _patsIdefinedInsts :: ([Identifier])
                    _patsIpatternAttributes :: ([(Identifier, Identifier)])
                    -- use rule "GenerateCode.ag"(line 195, column 55)
                    _lhsOdefinedInsts =
                        _patsIdefinedInsts
                    -- use rule "GenerateCode.ag"(line 202, column 47)
                    _lhsOpatternAttributes =
                        _patsIpatternAttributes
                    -- self rule
                    _copy =
                        Product pos_ _patsIcopy
                    -- self rule
                    _lhsOcopy =
                        _copy
                    ( _patsIcopy,_patsIdefinedInsts,_patsIpatternAttributes) =
                        (pats_ )
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore pos_  =
    (T_Pattern (let _lhsOdefinedInsts :: ([Identifier])
                    _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                    _lhsOcopy :: Pattern
                    -- use rule "GenerateCode.ag"(line 195, column 55)
                    _lhsOdefinedInsts =
                        []
                    -- use rule "GenerateCode.ag"(line 202, column 47)
                    _lhsOpatternAttributes =
                        []
                    -- self rule
                    _copy =
                        Underscore pos_
                    -- self rule
                    _lhsOcopy =
                        _copy
                in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      synthesized attributes:
         copy                 : SELF 
         definedInsts         : [Identifier]
         patternAttributes    : [(Identifier, Identifier)]
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
newtype T_Patterns  = T_Patterns (( Patterns,([Identifier]),([(Identifier, Identifier)])))
data Inh_Patterns  = Inh_Patterns {}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),definedInsts_Syn_Patterns :: !([Identifier]),patternAttributes_Syn_Patterns :: !([(Identifier, Identifier)])}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns (T_Patterns sem ) (Inh_Patterns )  =
    (let ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes) =
             (sem )
     in  (Syn_Patterns _lhsOcopy _lhsOdefinedInsts _lhsOpatternAttributes ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons (T_Pattern hd_ ) (T_Patterns tl_ )  =
    (T_Patterns (let _lhsOdefinedInsts :: ([Identifier])
                     _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                     _lhsOcopy :: Patterns
                     _hdIcopy :: Pattern
                     _hdIdefinedInsts :: ([Identifier])
                     _hdIpatternAttributes :: ([(Identifier, Identifier)])
                     _tlIcopy :: Patterns
                     _tlIdefinedInsts :: ([Identifier])
                     _tlIpatternAttributes :: ([(Identifier, Identifier)])
                     -- use rule "GenerateCode.ag"(line 195, column 55)
                     _lhsOdefinedInsts =
                         _hdIdefinedInsts ++ _tlIdefinedInsts
                     -- use rule "GenerateCode.ag"(line 202, column 47)
                     _lhsOpatternAttributes =
                         _hdIpatternAttributes ++ _tlIpatternAttributes
                     -- self rule
                     _copy =
                         (:) _hdIcopy _tlIcopy
                     -- self rule
                     _lhsOcopy =
                         _copy
                     ( _hdIcopy,_hdIdefinedInsts,_hdIpatternAttributes) =
                         (hd_ )
                     ( _tlIcopy,_tlIdefinedInsts,_tlIpatternAttributes) =
                         (tl_ )
                 in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (let _lhsOdefinedInsts :: ([Identifier])
                     _lhsOpatternAttributes :: ([(Identifier, Identifier)])
                     _lhsOcopy :: Patterns
                     -- use rule "GenerateCode.ag"(line 195, column 55)
                     _lhsOdefinedInsts =
                         []
                     -- use rule "GenerateCode.ag"(line 202, column 47)
                     _lhsOpatternAttributes =
                         []
                     -- self rule
                     _copy =
                         []
                     -- self rule
                     _lhsOcopy =
                         _copy
                 in  ( _lhsOcopy,_lhsOdefinedInsts,_lhsOpatternAttributes)) )
-- Sequence ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allNts               : Set NontermIdent
         children             : [(Identifier,Type,Bool)]
         con                  : ConstructorIdent
         higherOrderChildren  : [(Identifier,Type,Bool)]
         inh                  : Attributes
         instVisitNrs         : Map Identifier Int
         lastExpr             : Expr
         nr                   : Int
         nt                   : NontermIdent
         o_case               : Bool
         o_cata               : Bool
         o_costcentre         : Bool
         o_data               : Maybe Bool
         o_linePragmas        : Bool
         o_newtypes           : Bool
         o_pretty             : Bool
         o_rename             : Bool
         o_sem                : Bool
         o_sig                : Bool
         o_splitsems          : Bool
         o_strictwrap         : Bool
         o_traces             : Bool
         o_unbox              : Bool
         paramInstMap         : Map Identifier (NontermIdent, [String])
         paramMap             : ParamMap
         prefix               : String
         syn                  : Attributes
         terminals            : [Identifier]
         unfoldSemDom         : NontermIdent -> Int -> [String] -> Code.Type
         what                 : String
      chained attributes:
         declsAbove           : [Decl]
         errors               : Seq Error
      synthesized attributes:
         allTpsFound          : Bool
         blockDecls           : DeclBlocks 
         comments             : [String]
         decls                : Decls
         definedInsts         : [Identifier]
         exprs                : Exprs
         tSigs                : [Decl]
         tps                  : [Type]
         usedVars             : Set String
   alternatives:
      alternative Cons:
         child hd             : CRule 
         child tl             : Sequence 
      alternative Nil:
-}
-- cata
sem_Sequence :: Sequence  ->
                T_Sequence 
sem_Sequence list  =
    (Prelude.foldr sem_Sequence_Cons sem_Sequence_Nil (Prelude.map sem_CRule list) )
-- semantic domain
newtype T_Sequence  = T_Sequence ((Set NontermIdent) ->
                                  ([(Identifier,Type,Bool)]) ->
                                  ConstructorIdent ->
                                  ([Decl]) ->
                                  (Seq Error) ->
                                  ([(Identifier,Type,Bool)]) ->
                                  Attributes ->
                                  (Map Identifier Int) ->
                                  Expr ->
                                  Int ->
                                  NontermIdent ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  (Maybe Bool) ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  Bool ->
                                  (Map Identifier (NontermIdent, [String])) ->
                                  ParamMap ->
                                  String ->
                                  Attributes ->
                                  ([Identifier]) ->
                                  (NontermIdent -> Int -> [String] -> Code.Type) ->
                                  String ->
                                  ( Bool,DeclBlocks,([String]),Decls,([Decl]),([Identifier]),(Seq Error),Exprs,([Decl]),([Type]),(Set String)))
data Inh_Sequence  = Inh_Sequence {allNts_Inh_Sequence :: !(Set NontermIdent),children_Inh_Sequence :: !([(Identifier,Type,Bool)]),con_Inh_Sequence :: !(ConstructorIdent),declsAbove_Inh_Sequence :: !([Decl]),errors_Inh_Sequence :: !(Seq Error),higherOrderChildren_Inh_Sequence :: !([(Identifier,Type,Bool)]),inh_Inh_Sequence :: !(Attributes),instVisitNrs_Inh_Sequence :: !(Map Identifier Int),lastExpr_Inh_Sequence :: !(Expr),nr_Inh_Sequence :: !(Int),nt_Inh_Sequence :: !(NontermIdent),o_case_Inh_Sequence :: !(Bool),o_cata_Inh_Sequence :: !(Bool),o_costcentre_Inh_Sequence :: !(Bool),o_data_Inh_Sequence :: !(Maybe Bool),o_linePragmas_Inh_Sequence :: !(Bool),o_newtypes_Inh_Sequence :: !(Bool),o_pretty_Inh_Sequence :: !(Bool),o_rename_Inh_Sequence :: !(Bool),o_sem_Inh_Sequence :: !(Bool),o_sig_Inh_Sequence :: !(Bool),o_splitsems_Inh_Sequence :: !(Bool),o_strictwrap_Inh_Sequence :: !(Bool),o_traces_Inh_Sequence :: !(Bool),o_unbox_Inh_Sequence :: !(Bool),paramInstMap_Inh_Sequence :: !(Map Identifier (NontermIdent, [String])),paramMap_Inh_Sequence :: !(ParamMap),prefix_Inh_Sequence :: !(String),syn_Inh_Sequence :: !(Attributes),terminals_Inh_Sequence :: !([Identifier]),unfoldSemDom_Inh_Sequence :: !(NontermIdent -> Int -> [String] -> Code.Type),what_Inh_Sequence :: !(String)}
data Syn_Sequence  = Syn_Sequence {allTpsFound_Syn_Sequence :: !(Bool),blockDecls_Syn_Sequence :: !(DeclBlocks),comments_Syn_Sequence :: !([String]),decls_Syn_Sequence :: !(Decls),declsAbove_Syn_Sequence :: !([Decl]),definedInsts_Syn_Sequence :: !([Identifier]),errors_Syn_Sequence :: !(Seq Error),exprs_Syn_Sequence :: !(Exprs),tSigs_Syn_Sequence :: !([Decl]),tps_Syn_Sequence :: !([Type]),usedVars_Syn_Sequence :: !(Set String)}
wrap_Sequence :: T_Sequence  ->
                 Inh_Sequence  ->
                 Syn_Sequence 
wrap_Sequence (T_Sequence sem ) (Inh_Sequence _lhsIallNts _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIerrors _lhsIhigherOrderChildren _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwhat )  =
    (let ( _lhsOallTpsFound,_lhsOblockDecls,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOerrors,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars) =
             (sem _lhsIallNts _lhsIchildren _lhsIcon _lhsIdeclsAbove _lhsIerrors _lhsIhigherOrderChildren _lhsIinh _lhsIinstVisitNrs _lhsIlastExpr _lhsInr _lhsInt _lhsIo_case _lhsIo_cata _lhsIo_costcentre _lhsIo_data _lhsIo_linePragmas _lhsIo_newtypes _lhsIo_pretty _lhsIo_rename _lhsIo_sem _lhsIo_sig _lhsIo_splitsems _lhsIo_strictwrap _lhsIo_traces _lhsIo_unbox _lhsIparamInstMap _lhsIparamMap _lhsIprefix _lhsIsyn _lhsIterminals _lhsIunfoldSemDom _lhsIwhat )
     in  (Syn_Sequence _lhsOallTpsFound _lhsOblockDecls _lhsOcomments _lhsOdecls _lhsOdeclsAbove _lhsOdefinedInsts _lhsOerrors _lhsOexprs _lhsOtSigs _lhsOtps _lhsOusedVars ))
sem_Sequence_Cons :: T_CRule  ->
                     T_Sequence  ->
                     T_Sequence 
sem_Sequence_Cons (T_CRule hd_ ) (T_Sequence tl_ )  =
    (T_Sequence (\ _lhsIallNts
                   _lhsIchildren
                   _lhsIcon
                   _lhsIdeclsAbove
                   _lhsIerrors
                   _lhsIhigherOrderChildren
                   _lhsIinh
                   _lhsIinstVisitNrs
                   _lhsIlastExpr
                   _lhsInr
                   _lhsInt
                   _lhsIo_case
                   _lhsIo_cata
                   _lhsIo_costcentre
                   _lhsIo_data
                   _lhsIo_linePragmas
                   _lhsIo_newtypes
                   _lhsIo_pretty
                   _lhsIo_rename
                   _lhsIo_sem
                   _lhsIo_sig
                   _lhsIo_splitsems
                   _lhsIo_strictwrap
                   _lhsIo_traces
                   _lhsIo_unbox
                   _lhsIparamInstMap
                   _lhsIparamMap
                   _lhsIprefix
                   _lhsIsyn
                   _lhsIterminals
                   _lhsIunfoldSemDom
                   _lhsIwhat ->
                     (let _lhsOblockDecls :: DeclBlocks
                          _lhsOallTpsFound :: Bool
                          _lhsOcomments :: ([String])
                          _lhsOdecls :: Decls
                          _lhsOdefinedInsts :: ([Identifier])
                          _lhsOerrors :: (Seq Error)
                          _lhsOexprs :: Exprs
                          _lhsOtSigs :: ([Decl])
                          _lhsOtps :: ([Type])
                          _lhsOusedVars :: (Set String)
                          _lhsOdeclsAbove :: ([Decl])
                          _hdOallNts :: (Set NontermIdent)
                          _hdOchildren :: ([(Identifier,Type,Bool)])
                          _hdOcon :: ConstructorIdent
                          _hdOdeclsAbove :: ([Decl])
                          _hdOerrors :: (Seq Error)
                          _hdOhigherOrderChildren :: ([(Identifier,Type,Bool)])
                          _hdOinh :: Attributes
                          _hdOinstVisitNrs :: (Map Identifier Int)
                          _hdOnr :: Int
                          _hdOnt :: NontermIdent
                          _hdOo_case :: Bool
                          _hdOo_cata :: Bool
                          _hdOo_costcentre :: Bool
                          _hdOo_data :: (Maybe Bool)
                          _hdOo_linePragmas :: Bool
                          _hdOo_newtypes :: Bool
                          _hdOo_pretty :: Bool
                          _hdOo_rename :: Bool
                          _hdOo_sem :: Bool
                          _hdOo_sig :: Bool
                          _hdOo_splitsems :: Bool
                          _hdOo_strictwrap :: Bool
                          _hdOo_traces :: Bool
                          _hdOo_unbox :: Bool
                          _hdOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                          _hdOparamMap :: ParamMap
                          _hdOprefix :: String
                          _hdOsyn :: Attributes
                          _hdOterminals :: ([Identifier])
                          _hdOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                          _hdOwhat :: String
                          _tlOallNts :: (Set NontermIdent)
                          _tlOchildren :: ([(Identifier,Type,Bool)])
                          _tlOcon :: ConstructorIdent
                          _tlOdeclsAbove :: ([Decl])
                          _tlOerrors :: (Seq Error)
                          _tlOhigherOrderChildren :: ([(Identifier,Type,Bool)])
                          _tlOinh :: Attributes
                          _tlOinstVisitNrs :: (Map Identifier Int)
                          _tlOlastExpr :: Expr
                          _tlOnr :: Int
                          _tlOnt :: NontermIdent
                          _tlOo_case :: Bool
                          _tlOo_cata :: Bool
                          _tlOo_costcentre :: Bool
                          _tlOo_data :: (Maybe Bool)
                          _tlOo_linePragmas :: Bool
                          _tlOo_newtypes :: Bool
                          _tlOo_pretty :: Bool
                          _tlOo_rename :: Bool
                          _tlOo_sem :: Bool
                          _tlOo_sig :: Bool
                          _tlOo_splitsems :: Bool
                          _tlOo_strictwrap :: Bool
                          _tlOo_traces :: Bool
                          _tlOo_unbox :: Bool
                          _tlOparamInstMap :: (Map Identifier (NontermIdent, [String]))
                          _tlOparamMap :: ParamMap
                          _tlOprefix :: String
                          _tlOsyn :: Attributes
                          _tlOterminals :: ([Identifier])
                          _tlOunfoldSemDom :: (NontermIdent -> Int -> [String] -> Code.Type)
                          _tlOwhat :: String
                          _hdIallTpsFound :: Bool
                          _hdIbldBlocksFun :: (DeclBlocks -> DeclBlocks)
                          _hdIcomments :: ([String])
                          _hdIdecls :: Decls
                          _hdIdeclsAbove :: ([Decl])
                          _hdIdefinedInsts :: ([Identifier])
                          _hdIerrors :: (Seq Error)
                          _hdIexprs :: Exprs
                          _hdItSigs :: ([Decl])
                          _hdItps :: ([Type])
                          _hdIusedVars :: (Set String)
                          _tlIallTpsFound :: Bool
                          _tlIblockDecls :: DeclBlocks
                          _tlIcomments :: ([String])
                          _tlIdecls :: Decls
                          _tlIdeclsAbove :: ([Decl])
                          _tlIdefinedInsts :: ([Identifier])
                          _tlIerrors :: (Seq Error)
                          _tlIexprs :: Exprs
                          _tlItSigs :: ([Decl])
                          _tlItps :: ([Type])
                          _tlIusedVars :: (Set String)
                          -- "GenerateCode.ag"(line 476, column 7)
                          _lhsOblockDecls =
                              _hdIbldBlocksFun _tlIblockDecls
                          -- use rule "GenerateCode.ag"(line 344, column 39)
                          _lhsOallTpsFound =
                              _hdIallTpsFound && _tlIallTpsFound
                          -- use rule "GenerateCode.ag"(line 738, column 52)
                          _lhsOcomments =
                              _hdIcomments ++ _tlIcomments
                          -- use rule "GenerateCode.ag"(line 138, column 34)
                          _lhsOdecls =
                              _hdIdecls ++ _tlIdecls
                          -- use rule "GenerateCode.ag"(line 195, column 55)
                          _lhsOdefinedInsts =
                              _hdIdefinedInsts ++ _tlIdefinedInsts
                          -- use rule "GenerateCode.ag"(line 719, column 32)
                          _lhsOerrors =
                              _hdIerrors Seq.>< _tlIerrors
                          -- use rule "GenerateCode.ag"(line 268, column 34)
                          _lhsOexprs =
                              _hdIexprs ++ _tlIexprs
                          -- use rule "GenerateCode.ag"(line 297, column 33)
                          _lhsOtSigs =
                              _hdItSigs ++ _tlItSigs
                          -- use rule "GenerateCode.ag"(line 343, column 31)
                          _lhsOtps =
                              _hdItps ++ _tlItps
                          -- use rule "GenerateCode.ag"(line 286, column 37)
                          _lhsOusedVars =
                              _hdIusedVars `Set.union` _tlIusedVars
                          -- copy rule (up)
                          _lhsOdeclsAbove =
                              _tlIdeclsAbove
                          -- copy rule (down)
                          _hdOallNts =
                              _lhsIallNts
                          -- copy rule (down)
                          _hdOchildren =
                              _lhsIchildren
                          -- copy rule (down)
                          _hdOcon =
                              _lhsIcon
                          -- copy rule (down)
                          _hdOdeclsAbove =
                              _lhsIdeclsAbove
                          -- copy rule (down)
                          _hdOerrors =
                              _lhsIerrors
                          -- copy rule (down)
                          _hdOhigherOrderChildren =
                              _lhsIhigherOrderChildren
                          -- copy rule (down)
                          _hdOinh =
                              _lhsIinh
                          -- copy rule (down)
                          _hdOinstVisitNrs =
                              _lhsIinstVisitNrs
                          -- copy rule (down)
                          _hdOnr =
                              _lhsInr
                          -- copy rule (down)
                          _hdOnt =
                              _lhsInt
                          -- copy rule (down)
                          _hdOo_case =
                              _lhsIo_case
                          -- copy rule (down)
                          _hdOo_cata =
                              _lhsIo_cata
                          -- copy rule (down)
                          _hdOo_costcentre =
                              _lhsIo_costcentre
                          -- copy rule (down)
                          _hdOo_data =
                              _lhsIo_data
                          -- copy rule (down)
                          _hdOo_linePragmas =
                              _lhsIo_linePragmas
                          -- copy rule (down)
                          _hdOo_newtypes =
                              _lhsIo_newtypes
                          -- copy rule (down)
                          _hdOo_pretty =
                              _lhsIo_pretty
                          -- copy rule (down)
                          _hdOo_rename =
                              _lhsIo_rename
                          -- copy rule (down)
                          _hdOo_sem =
                              _lhsIo_sem
                          -- copy rule (down)
                          _hdOo_sig =
                              _lhsIo_sig
                          -- copy rule (down)
                          _hdOo_splitsems =
                              _lhsIo_splitsems
                          -- copy rule (down)
                          _hdOo_strictwrap =
                              _lhsIo_strictwrap
                          -- copy rule (down)
                          _hdOo_traces =
                              _lhsIo_traces
                          -- copy rule (down)
                          _hdOo_unbox =
                              _lhsIo_unbox
                          -- copy rule (down)
                          _hdOparamInstMap =
                              _lhsIparamInstMap
                          -- copy rule (down)
                          _hdOparamMap =
                              _lhsIparamMap
                          -- copy rule (down)
                          _hdOprefix =
                              _lhsIprefix
                          -- copy rule (down)
                          _hdOsyn =
                              _lhsIsyn
                          -- copy rule (down)
                          _hdOterminals =
                              _lhsIterminals
                          -- copy rule (down)
                          _hdOunfoldSemDom =
                              _lhsIunfoldSemDom
                          -- copy rule (down)
                          _hdOwhat =
                              _lhsIwhat
                          -- copy rule (down)
                          _tlOallNts =
                              _lhsIallNts
                          -- copy rule (down)
                          _tlOchildren =
                              _lhsIchildren
                          -- copy rule (down)
                          _tlOcon =
                              _lhsIcon
                          -- copy rule (chain)
                          _tlOdeclsAbove =
                              _hdIdeclsAbove
                          -- copy rule (chain)
                          _tlOerrors =
                              _hdIerrors
                          -- copy rule (down)
                          _tlOhigherOrderChildren =
                              _lhsIhigherOrderChildren
                          -- copy rule (down)
                          _tlOinh =
                              _lhsIinh
                          -- copy rule (down)
                          _tlOinstVisitNrs =
                              _lhsIinstVisitNrs
                          -- copy rule (down)
                          _tlOlastExpr =
                              _lhsIlastExpr
                          -- copy rule (down)
                          _tlOnr =
                              _lhsInr
                          -- copy rule (down)
                          _tlOnt =
                              _lhsInt
                          -- copy rule (down)
                          _tlOo_case =
                              _lhsIo_case
                          -- copy rule (down)
                          _tlOo_cata =
                              _lhsIo_cata
                          -- copy rule (down)
                          _tlOo_costcentre =
                              _lhsIo_costcentre
                          -- copy rule (down)
                          _tlOo_data =
                              _lhsIo_data
                          -- copy rule (down)
                          _tlOo_linePragmas =
                              _lhsIo_linePragmas
                          -- copy rule (down)
                          _tlOo_newtypes =
                              _lhsIo_newtypes
                          -- copy rule (down)
                          _tlOo_pretty =
                              _lhsIo_pretty
                          -- copy rule (down)
                          _tlOo_rename =
                              _lhsIo_rename
                          -- copy rule (down)
                          _tlOo_sem =
                              _lhsIo_sem
                          -- copy rule (down)
                          _tlOo_sig =
                              _lhsIo_sig
                          -- copy rule (down)
                          _tlOo_splitsems =
                              _lhsIo_splitsems
                          -- copy rule (down)
                          _tlOo_strictwrap =
                              _lhsIo_strictwrap
                          -- copy rule (down)
                          _tlOo_traces =
                              _lhsIo_traces
                          -- copy rule (down)
                          _tlOo_unbox =
                              _lhsIo_unbox
                          -- copy rule (down)
                          _tlOparamInstMap =
                              _lhsIparamInstMap
                          -- copy rule (down)
                          _tlOparamMap =
                              _lhsIparamMap
                          -- copy rule (down)
                          _tlOprefix =
                              _lhsIprefix
                          -- copy rule (down)
                          _tlOsyn =
                              _lhsIsyn
                          -- copy rule (down)
                          _tlOterminals =
                              _lhsIterminals
                          -- copy rule (down)
                          _tlOunfoldSemDom =
                              _lhsIunfoldSemDom
                          -- copy rule (down)
                          _tlOwhat =
                              _lhsIwhat
                          ( _hdIallTpsFound,_hdIbldBlocksFun,_hdIcomments,_hdIdecls,_hdIdeclsAbove,_hdIdefinedInsts,_hdIerrors,_hdIexprs,_hdItSigs,_hdItps,_hdIusedVars) =
                              (hd_ _hdOallNts _hdOchildren _hdOcon _hdOdeclsAbove _hdOerrors _hdOhigherOrderChildren _hdOinh _hdOinstVisitNrs _hdOnr _hdOnt _hdOo_case _hdOo_cata _hdOo_costcentre _hdOo_data _hdOo_linePragmas _hdOo_newtypes _hdOo_pretty _hdOo_rename _hdOo_sem _hdOo_sig _hdOo_splitsems _hdOo_strictwrap _hdOo_traces _hdOo_unbox _hdOparamInstMap _hdOparamMap _hdOprefix _hdOsyn _hdOterminals _hdOunfoldSemDom _hdOwhat )
                          ( _tlIallTpsFound,_tlIblockDecls,_tlIcomments,_tlIdecls,_tlIdeclsAbove,_tlIdefinedInsts,_tlIerrors,_tlIexprs,_tlItSigs,_tlItps,_tlIusedVars) =
                              (tl_ _tlOallNts _tlOchildren _tlOcon _tlOdeclsAbove _tlOerrors _tlOhigherOrderChildren _tlOinh _tlOinstVisitNrs _tlOlastExpr _tlOnr _tlOnt _tlOo_case _tlOo_cata _tlOo_costcentre _tlOo_data _tlOo_linePragmas _tlOo_newtypes _tlOo_pretty _tlOo_rename _tlOo_sem _tlOo_sig _tlOo_splitsems _tlOo_strictwrap _tlOo_traces _tlOo_unbox _tlOparamInstMap _tlOparamMap _tlOprefix _tlOsyn _tlOterminals _tlOunfoldSemDom _tlOwhat )
                      in  ( _lhsOallTpsFound,_lhsOblockDecls,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOerrors,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars))) )
sem_Sequence_Nil :: T_Sequence 
sem_Sequence_Nil  =
    (T_Sequence (\ _lhsIallNts
                   _lhsIchildren
                   _lhsIcon
                   _lhsIdeclsAbove
                   _lhsIerrors
                   _lhsIhigherOrderChildren
                   _lhsIinh
                   _lhsIinstVisitNrs
                   _lhsIlastExpr
                   _lhsInr
                   _lhsInt
                   _lhsIo_case
                   _lhsIo_cata
                   _lhsIo_costcentre
                   _lhsIo_data
                   _lhsIo_linePragmas
                   _lhsIo_newtypes
                   _lhsIo_pretty
                   _lhsIo_rename
                   _lhsIo_sem
                   _lhsIo_sig
                   _lhsIo_splitsems
                   _lhsIo_strictwrap
                   _lhsIo_traces
                   _lhsIo_unbox
                   _lhsIparamInstMap
                   _lhsIparamMap
                   _lhsIprefix
                   _lhsIsyn
                   _lhsIterminals
                   _lhsIunfoldSemDom
                   _lhsIwhat ->
                     (let _lhsOblockDecls :: DeclBlocks
                          _lhsOallTpsFound :: Bool
                          _lhsOcomments :: ([String])
                          _lhsOdecls :: Decls
                          _lhsOdefinedInsts :: ([Identifier])
                          _lhsOerrors :: (Seq Error)
                          _lhsOexprs :: Exprs
                          _lhsOtSigs :: ([Decl])
                          _lhsOtps :: ([Type])
                          _lhsOusedVars :: (Set String)
                          _lhsOdeclsAbove :: ([Decl])
                          -- "GenerateCode.ag"(line 478, column 7)
                          _lhsOblockDecls =
                              DeclTerminator _lhsIdeclsAbove _lhsIlastExpr
                          -- use rule "GenerateCode.ag"(line 344, column 39)
                          _lhsOallTpsFound =
                              True
                          -- use rule "GenerateCode.ag"(line 738, column 52)
                          _lhsOcomments =
                              []
                          -- use rule "GenerateCode.ag"(line 138, column 34)
                          _lhsOdecls =
                              []
                          -- use rule "GenerateCode.ag"(line 195, column 55)
                          _lhsOdefinedInsts =
                              []
                          -- use rule "GenerateCode.ag"(line 719, column 32)
                          _lhsOerrors =
                              Seq.empty
                          -- use rule "GenerateCode.ag"(line 268, column 34)
                          _lhsOexprs =
                              []
                          -- use rule "GenerateCode.ag"(line 297, column 33)
                          _lhsOtSigs =
                              []
                          -- use rule "GenerateCode.ag"(line 343, column 31)
                          _lhsOtps =
                              []
                          -- use rule "GenerateCode.ag"(line 286, column 37)
                          _lhsOusedVars =
                              Set.empty
                          -- copy rule (chain)
                          _lhsOdeclsAbove =
                              _lhsIdeclsAbove
                      in  ( _lhsOallTpsFound,_lhsOblockDecls,_lhsOcomments,_lhsOdecls,_lhsOdeclsAbove,_lhsOdefinedInsts,_lhsOerrors,_lhsOexprs,_lhsOtSigs,_lhsOtps,_lhsOusedVars))) )