module EvaluateTypes where

import DocTypes

import DebugLevels
import IOExts -- evaluate has IO, so the unsafePerformIO is only temporary
import Char
import Data.FiniteMap

import DocumentEdit

import UHA_Syntax

--import qualified ExtractImportDecls   (sem_Module)
--import qualified StaticAnalysis       (sem_Module)
import CompileHelium
import Utils
--import SAMessages
import StaticErrors
import TypeErrors
import Top.Types
--import MyAssocList
import UHA_Utils
import UHA_Range
import HeliumMessages

import qualified UHA_Pretty       (sem_Module, sem_Declarations)


ppUHADoc :: Document -> String
ppUHADoc doc = show . UHA_Pretty.sem_Module . uhaFromDoc $ doc

ppDeclarations :: Declarations -> String
ppDeclarations decls = show $ UHA_Pretty.sem_Declarations decls



readM         :: Read a => String -> Maybe a
readM s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
            [x] -> Just x
            _   -> Nothing

pathFromRange :: Range -> PathDoc
pathFromRange (Range_Range (Position_Position pathStr@(_:_) _ _) _) =
  case readM pathStr of
    Just path -> PathD path -- Nothing is when identifier is from other module
    Nothing   -> {-debug Err ("EvaluateTypes.pathFromRange Incorrect range "++show pathStr)-} NoPathD
pathFromRange _                                                     = NoPathD

evaluate :: Document -> ([HeliumMessage], [(PathDoc,String)], [(String,String)])
evaluate doc = henk2 . uhaFromDoc $ doc


-- The path info is the path in Proxima's Document. It is stored in the filename string of range,
-- so ranges from UHA can be mapped back onto Proxima Document locations

uhaFromDoc :: Document -> Module
uhaFromDoc (RootDoc _ _ list_decl) = Module_Module
                                   (range []) MaybeName_Nothing MaybeExports_Nothing
                                   (Body_Body (range []) [] (uhaFromList_Decl [] list_decl))


uhaFromList_Decl :: [Int] -> List_Decl -> [Declaration]
uhaFromList_Decl pth (List_Decl _ consList) = concat [ uhaFromDecl (pth++[i]) dcl | (dcl,i) <- zip (fromConsList_Decl consList) [0..] ]
uhaFromList_Decl _ _ = []


uhaFromDecl :: [Int] -> Decl -> [Declaration] -- return type is List so wrong Decl's can return a []
uhaFromDecl pth (Decl _ _ _ _ _ _ _ ident exp) =
  [ Declaration_FunctionBindings (range pth)
    [ FunctionBinding_FunctionBinding (range pth)
      (LeftHandSide_Function (range (pth++[2])) (uhaFromIdent (pth++[2]) ident) [])
      (RightHandSide_Expression (range (pth++[3]))
                               (uhaFromExp (pth++[3]) exp)
                               MaybeDeclarations_Nothing)
    ]
  ]
uhaFromDecl pth (PPPresentationDecl _ _ _ pppres) = uhaFromPPPresentation (pth++[0]) pppres
uhaFromDecl _ _ = []


uhaFromIdent :: [Int] -> Ident -> Name
uhaFromIdent pth (Ident _ _ _ nm) = (Name_Identifier (range pth) [] (uhaFromString_ nm))
uhaFromIdent pth _              = (Name_Identifier (range pth) [] "x")

uhaFromExp :: [Int] -> Exp -> Expression
uhaFromExp pth (PlusExp _ _ exp1 exp2)     = mkInfixApp pth "+" exp1 exp2
uhaFromExp pth (TimesExp _ _ exp1 exp2)    = mkInfixApp pth "*" exp1 exp2
uhaFromExp pth (DivExp _ _ exp1 exp2)      = mkInfixApp pth "div" exp1 exp2
uhaFromExp pth (PowerExp _ _ exp1 exp2)    = mkInfixApp pth "^" exp1 exp2
uhaFromExp pth (BoolExp _ _ bool_)          = Expression_Constructor (range pth)
                                             $ Name_Special (range pth) [] (show $ uhaFromBool_ bool_)
uhaFromExp pth (IntExp _ _ int_)            = Expression_Literal (range pth)
                                             $ Literal_Int (range pth) (show $ uhaFromInt_ int_)
uhaFromExp pth (LamExp _ _ _ ident exp)    = Expression_Lambda (range pth)
                                               [Pattern_Variable (range (pth++[0]))
                                                                 (uhaFromIdent (pth++[0]) ident) ]
                                               (uhaFromExp (pth++[1]) exp)
uhaFromExp pth (CaseExp _ _ _ exp list_alt)    = Expression_Case (range pth)
                                               (uhaFromExp (pth++[0]) exp)
                                               (uhaFromList_Alt (pth++[1]) list_alt)
uhaFromExp pth (LetExp _ _ _ list_decl exp)    = Expression_Let (range pth)
                                               (uhaFromList_Decl (pth++[0]) list_decl)
                                               (uhaFromExp (pth++[1]) exp)
uhaFromExp pth (AppExp _ exp1 exp2)        = Expression_NormalApplication (range pth)
                                               (uhaFromExp (pth++[0]) exp1)
                                               [uhaFromExp (pth++[1]) exp2]
uhaFromExp pth (IdentExp _ ident)          = Expression_Variable (range pth)
                                             $ uhaFromIdent (pth++[0]) ident
uhaFromExp pth (IfExp _ _ _ _ exp1 exp2 exp3) = Expression_If (range pth)
                                                              (uhaFromExp (pth++[0]) exp1)
                                                              (uhaFromExp (pth++[1]) exp2)
                                                              (uhaFromExp (pth++[2]) exp3)
uhaFromExp pth (ParenExp _ _ _ exp)        = Expression_Parenthesized (range pth)
                                             $ uhaFromExp (pth++[0]) exp
uhaFromExp pth (ListExp _ _ _ _ list_exp)      = Expression_List (range pth)
                                             $ uhaFromList_Exp (pth++[0]) list_exp
uhaFromExp pth (ProductExp _ _ _ _ list_exp)   = Expression_Tuple (range pth)
                                             $ uhaFromList_Exp (pth++[0]) list_exp
uhaFromExp pth HoleExp                     = Expression_Variable (range pth)
                                             $ Name_Identifier (range pth) [] "undefined"
uhaFromExp pth (ParseErrExp _ _)           = Expression_Variable (range pth)
                                             $ Name_Identifier (range pth) [] "undefined"
uhaFromExp pth  _                          = Expression_Variable (range pth)
                                             $ Name_Identifier (range pth) [] "undefined"

uhaFromList_Exp :: [Int] -> List_Exp -> [Expression]
uhaFromList_Exp pth (List_Exp _ consList) = [ uhaFromExp (pth++[i]) dcl | (dcl,i) <- zip (fromConsList_Exp consList) [0..] ]
uhaFromList_Exp _ _ = []


uhaFromList_Alt :: [Int] -> List_Alt -> [Alternative]
uhaFromList_Alt pth (List_Alt _ consList) = concat [ uhaFromAlt (pth++[i]) dcl | (dcl,i) <- zip (fromConsList_Alt consList) [0..] ]
uhaFromList_Alt _ _ = []

uhaFromAlt :: [Int] -> Alt -> [Alternative] -- return type is List so wrong Alt's can return a []
uhaFromAlt pth (Alt _ _ _ ident exp) = [ Alternative_Alternative (range pth)
                                            (Pattern_Variable (range (pth++[0]))
                                                              (uhaFromIdent (pth++[0]) ident))

                                            (RightHandSide_Expression (range (pth++[1]))
                                                                          (uhaFromExp (pth++[1]) exp)
                                                                          MaybeDeclarations_Nothing)

                                      ]
uhaFromAlt _ _ = []


-- collect the expressions in helium items and bind them to unique function names
uhaFromPPPresentation pth (PPPresentation _ vwtype list_slide) = uhaFromList_Slide (pth++[1]) list_slide
uhaFromPPPresentation _ _ = []


uhaFromList_Slide :: [Int] -> List_Slide -> [Declaration]
uhaFromList_Slide pth (List_Slide _ consList) = concat [ uhaFromSlide (pth++[i]) dcl | (dcl,i) <- zip (fromConsList_Slide consList) [0..] ]
uhaFromList_Slide _ _ = []


uhaFromSlide pth (Slide _ _ itemlist) = uhaFromItemList (pth++[1]) itemlist
uhaFromSlide _ _ = []

uhaFromItemList pth (ItemList _ lsttype list_item) = uhaFromList_Item (pth++[1]) list_item
uhaFromItemList _ _ = []

uhaFromList_Item :: [Int] -> List_Item -> [Declaration]
uhaFromList_Item pth (List_Item _ consList) = concat [ uhaFromItem (pth++[i]) dcl | (dcl,i) <- zip (fromConsList_Item consList) [0..] ]
uhaFromList_Item _ _ = []


uhaFromItem pth (HeliumItem _ exp) =
  [ Declaration_FunctionBindings noRange
    [
    FunctionBinding_FunctionBinding noRange
      (LeftHandSide_Function noRange (Name_Identifier noRange [] ("HeliumItem"++uniqueName pth)) [])
      (RightHandSide_Expression noRange
                               (uhaFromExp (pth++[0]) exp)
                               MaybeDeclarations_Nothing)
    ]
  ]
uhaFromItem _ _ = []



--
uhaFromString_ (String_ _ str) = str
uhaFromBool_   (Bool_ _ bool)  = bool
uhaFromInt_    (Int_ _ int)    = int




uniqueName pth = [ if isDigit c then c else '_' | c <- show pth  ]

mkInfixApp pth  op exp1 exp2 = Expression_InfixApplication (range pth)
                                 (MaybeExpression_Just $ uhaFromExp (pth++[0]) exp1)
                                 (Expression_Variable (range pth) (Name_Operator (range pth) [] op))
                                 (MaybeExpression_Just $ uhaFromExp (pth++[1]) exp2)

range :: [Int] -> Range
range pth = Range_Range (Position_Position (show pth) (-1) (-1)) Position_Unknown



--                  errors  type env             toplevel env


henk2 :: Module -> ([HeliumMessage], [(PathDoc,String)], [(String,String)])
henk2 mod =
  case unsafePerformIO $ do { debugLnIO Prs  "Helium compiler start type check"
                            ; errs <- compileHelium mod
                            ; debugLnIO Prs  "Helium compiler finish type check"
                            ; return errs
                            } of
    Left staticErrs -> debug Prs (show (map showMessage staticErrs)) (map hErrFromStaticErr staticErrs, [], [])
    Right ([], (types,typeEnv)) ->
      let typeEnv'  =[ (pathFromRange r, show t) | (r,t)<- typeEnv ]
          toplvlEnv = [ (getNameName nm,show tp) | (nm,tp) <- fmToList types ]
      in  ( [], typeEnv', toplvlEnv )
    Right (typeErrs, _) ->  (map hErrFromTypeErr typeErrs,[] ,[])


hErrFromStaticErr e@(NoFunDef entity name names)           = HError (lines $ showMessage e) [] (pathFromName name : map (pathFromName) names) []
hErrFromStaticErr e@(Undefined entity name names hints)    = debug Prs (show names) $ HError (lines $ showMessage e) [pathFromName name]  (map (pathFromName) names) []
hErrFromStaticErr e@(Duplicated entity names)              = HError (lines $ showMessage e) (map (pathFromName) names) [] []
hErrFromStaticErr e@(LastStatementNotExpr range)           = HError (lines $ showMessage e) [pathFromRange range] [] []
hErrFromStaticErr e@(WrongFileName string string' range)    = HError (lines $ showMessage e) [pathFromRange range] [] []
hErrFromStaticErr e@(TypeVarApplication name)              = HError (lines $ showMessage e) [pathFromName name] [] []
hErrFromStaticErr e@(ArityMismatch entity name int int')  = HError (lines $ showMessage e) [pathFromName name] [] []
hErrFromStaticErr e@(DefArityMismatch name maybeint range) = HError (lines$ showMessage e) [pathFromName name] [pathFromRange range] []
hErrFromStaticErr e@(RecursiveTypeSynonyms names)          = HError (lines $ showMessage e) (map (pathFromName) names) [] []
hErrFromStaticErr e@(PatternDefinesNoVars range)           = HError (lines $ showMessage e) [pathFromRange range] [] []
hErrFromStaticErr err = HError ("Unknown Static Error" : lines (showMessage err)) [] [] []

--TypeError = TypeError Bool String Range SourceDocs (Maybe (Bool,TpScheme),Tp,Tp) Hint
--               | NotGeneralEnough TpScheme TpScheme (Tree,Range) deriving Show
hErrFromTypeErr e@(TypeError ranges _ _ _) = HError (lines $ showMessage e) [] [] (map pathFromRange ranges)
hErrFromTypeErr err = HError ("Unhandled Type Error" : lines (showMessage err)) [] [] []


pathFromName = pathFromRange.getNameRange