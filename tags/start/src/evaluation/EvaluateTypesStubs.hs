module EvaluateTypesStubs where

-- copy of EvaluateTypes that does not import the Helium modules

import DocTypes

import DebugLevels
import IOExts -- evaluate has IO, so the unsafePerformIO is only temporary

import CompileHelium

import UHA_Syntax

{-
import qualified ExtractImportDecls   (sem_Module)
import qualified StaticAnalysis       (sem_Module)
import Helium
import Args
import SAMessages
import SATypes
import MyAssocList
import UHA_Utils

import qualified PrettyPrinting       (sem_Module)

-- uncomment and take other ppUHADoc and other henk (at the bottom) for helium type evaluation

-}


--ppUHADoc :: Document -> String
--ppUHADoc doc = show . PrettyPrinting.sem_Module . uhaFromDoc $ doc



readM         :: Read a => String -> Maybe a
readM s        =  case [x | (x,t) <- reads s, ("","") <- lex t] of
	 	    [x] -> Just x
	 	    _   -> Nothing

pathFromRange :: Range -> PathDoc
pathFromRange (Range_Range (Position_Position pathStr@(_:_) _ _) _) =
  case readM pathStr of
    Just path -> PathD path
    Nothing   -> debug Err ("EvaluateTypes.pathFromRange Incorrect range "++show pathStr) NoPathD
pathFromRange _                                                     = NoPathD

evaluate :: Document -> ([HeliumMessage], [(PathDoc,String)], [(String,String)])
evaluate doc = henk2 . uhaFromDoc $ doc



-- The path info is the path in Proxima's Document. It is stored in the filename string of range,
-- so ranges from UHA can be mapped back onto Proxima Document locations

uhaFromDoc :: Document -> Module
uhaFromDoc (RootDoc _ _ decls) = Module_Module 
                                   (range []) MaybeName_Nothing MaybeExports_Nothing
                                   (Body_Body (range []) [] (uhaFromDecls 0 [] decls))  


uhaFromDecls :: Int -> [Int] -> Decls -> [Declaration]
uhaFromDecls i pth (ConsDecls _ decl decls) = uhaFromDecl (pth++[i]) decl ++ uhaFromDecls (i+1) pth decls
uhaFromDecls i pth (NilDecls _) = [] 
uhaFromDecls _ _ _ = []


uhaFromDecl :: [Int] -> Decl -> [Declaration] -- return type is List so wrong Decl's can return a []
uhaFromDecl pth (Decl _ _ _ _ _ _ _ ident exp) = [ Declaration_FunctionBindings (range pth) 
                                            [ FunctionBinding_FunctionBinding (range pth)
                                                (LeftHandSide_Function (range (pth++[0])) (uhaFromIdent (pth++[0]) ident) [])
                                                (RightHandSide_Expression (range (pth++[1]))
                                                                          (uhaFromExp (pth++[1]) exp)
                                                                          MaybeDeclarations_Nothing)
                                            ]
                                      ]
uhaFromDecl _ _ = []                                      

uhaFromIdent :: [Int] -> Ident -> Name
uhaFromIdent pth (Ident _ _ _ nm) = (Name_Identifier (range pth) [] nm)

uhaFromExp :: [Int] -> Exp -> Expression
uhaFromExp pth (PlusExp _ _ exp1 exp2)     = mkInfixApp pth "+" exp1 exp2
uhaFromExp pth (TimesExp _ _ exp1 exp2)    = mkInfixApp pth "*" exp1 exp2
uhaFromExp pth (DivExp _ _ exp1 exp2)      = mkInfixApp pth "/" exp1 exp2
uhaFromExp pth (PowerExp _ _ exp1 exp2)    = mkInfixApp pth "^" exp1 exp2
uhaFromExp pth (BoolExp _ _ bool)          = Expression_Constructor (range pth) 
                                             $ Name_Special (range pth) [] (show bool)
uhaFromExp pth (IntExp _ _ int)            = Expression_Literal (range pth) 
                                             $ Literal_Int (range pth) (show int)
uhaFromExp pth (LamExp _ _ _ ident exp)    = Expression_Lambda (range pth) 
                                               [Pattern_Variable (range (pth++[0])) 
                                                                 (uhaFromIdent (pth++[0]) ident) ]
                                               (uhaFromExp (pth++[1]) exp)
uhaFromExp pth (CaseExp _ _ _ exp alts)    = Expression_Case (range pth) 
                                               (uhaFromExp (pth++[0]) exp)
                                               (uhaFromAlts 0 (pth++[1]) alts)                                                                 
uhaFromExp pth (LetExp _ _ _ decls exp)    = Expression_Let (range pth) 
                                               (uhaFromDecls 0 (pth++[0]) decls)                                                                 
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
uhaFromExp pth (ListExp _ _ _ _ exps)      = Expression_List (range pth)
                                             $ uhaFromExps 0 (pth++[0]) exps
uhaFromExp pth (ProductExp _ _ _ _ exps)   = Expression_Tuple (range pth)
                                             $ uhaFromExps 0 (pth++[0]) exps
uhaFromExp pth HoleExp                     = Expression_Variable (range pth)
                                             $ Name_Identifier (range pth) [] "undefined"
uhaFromExp pth (ParseErrExp _ _)           = Expression_Variable (range pth)
                                             $ Name_Identifier (range pth) [] "undefined"


uhaFromExps :: Int -> [Int] -> Exps -> [Expression]
uhaFromExps i pth (ConsExps _ exp exps) = uhaFromExp (pth++[i]) exp : uhaFromExps (i+1) pth exps
uhaFromExps i pth (NilExps _)           = [] 
uhaFromExps i pth _                     = [] 



uhaFromAlts :: Int -> [Int] -> Alts -> [Alternative]
uhaFromAlts i pth (ConsAlts _ decl decls) = uhaFromAlt (pth++[i]) decl ++ uhaFromAlts (i+1) pth decls
uhaFromAlts i pth (NilAlts _) = [] 
uhaFromAlts _ _ _ = []


uhaFromAlt :: [Int] -> Alt -> [Alternative] -- return type is List so wrong Alt's can return a []
uhaFromAlt pth (Alt _ _ _ ident exp) = [ Alternative_Alternative (range pth) 
                                            (Pattern_Variable (range (pth++[0]))
                                                              (uhaFromIdent (pth++[0]) ident))
                                                              
                                            (RightHandSide_Expression (range (pth++[1]))
                                                                          (uhaFromExp (pth++[1]) exp)
                                                                          MaybeDeclarations_Nothing)
                                            
                                      ]
uhaFromAlt _ _ = []                                      



mkInfixApp pth  op exp1 exp2 = Expression_InfixApplication (range pth) 
                                 (MaybeExpression_Just $ uhaFromExp (pth++[0]) exp1)
                                 (Expression_Variable (range pth) (Name_Operator (range pth) [] op))
                                 (MaybeExpression_Just $ uhaFromExp (pth++[1]) exp2) 

range :: [Int] -> Range
range pth = Range_Range (Position_Position (show pth) (-1) (-1)) Position_Unknown



--Helium stub
henk2 :: Module -> ([HeliumMessage], [(PathDoc,String)], [(String,String)])
henk2 _ = ([HError [ "Type checking not available, because Helium Compiler is not imported.",
                     "In EvalPresent, import EvaluateTypes instead of EvaluateTypeStubs."] [] [] []
           ], [], [])


{-

Helium binding



--                  errors  type env             toplevel env   
henk2 :: Module -> ([HeliumMessage], [(PathDoc,String)], [(String,String)])
henk2 mod = 
  case unsafePerformIO $ do { debugLnIO Prs  "Helium compiler start parse + type check"
                            ; errs <- compileOne mod [UseTypeGraph]
                            ; debugLnIO Prs  "Helium compiler finish parse + type check"
                            ; return errs
                            } of
    Left staticErrs -> debug Prs (show staticErrs) (map hErrFromStaticErr staticErrs, [], [])
    Right ([], (types, subst,typeEnv)) -> 
      let typeEnv'  = [ (pathFromRange r, show (generalizeAll  (subst |-> t))) | (r,t)<- typeEnv ]
          toplvlEnv = [ (getNameName nm,show tp) | (nm,tp) <- toList types ]
      in  ( [], typeEnv', toplvlEnv )
    Right (typeErrs, _) ->  (map hErrFromTypeErr typeErrs,[] ,[])

instance (Ord a, Show a, Show b) => Show (AssocList a b) where
  show assoclist = show $ toList assoclist
  
showtypes subst =
  concat [ show b ++ "  "++  show (generalizeAll  (subst |-> TVar b))  ++ "\n" | b<-[1..50] ]


hErrFromStaticErr e@(NoFunDef entity name names)           = HError (lines $ show e) [] (pathFromName name : map (pathFromName) names) []
hErrFromStaticErr e@(Undefined entity name names)          = debug Prs (show names) $ HError (lines $ show e) [pathFromName name]  (map (pathFromName) names) []
hErrFromStaticErr e@(Duplicated entity names)              = HError (lines $ show e) (map (pathFromName) names) [] []
hErrFromStaticErr e@(LastStatementNotExpr range)           = HError (lines $ show e) [pathFromRange range] [] []
hErrFromStaticErr e@(WrongFileName string string' range)    = HError (lines $ show e) [pathFromRange range] [] []
hErrFromStaticErr e@(TypeVarApplication name)              = HError (lines $ show e) [pathFromName name] [] [] 
hErrFromStaticErr e@(ArityMismatch entity name int int')  = HError (lines $ show e) [pathFromName name] [] [] 
hErrFromStaticErr e@(DefArityMismatch name maybeint range) = HError (lines$ show e) [pathFromName name] [pathFromRange range] []
hErrFromStaticErr e@(RecursiveTypeSynonyms names)          = HError (lines $ show e) (map (pathFromName) names) [] []
hErrFromStaticErr e@(PatternDefinesNoVars range)           = HError (lines $ show e) [pathFromRange range] [] []
hErrFromStaticErr err = HError ("Unknown Static Error" : lines (show err)) [] [] []

--TypeError = TypeError Bool String Range SourceDocs (Maybe (Bool,TpScheme),Tp,Tp) Hint
--               | NotGeneralEnough TpScheme TpScheme (Tree,Range) deriving Show
hErrFromTypeErr e@(TypeError _ _ range _ _ _) = HError (lines $ show e) [] [] [pathFromRange range] 
hErrFromTypeErr e@(NotGeneralEnough _ _ (_,range)) = HError (lines $ show e) [] [] [pathFromRange range]
hErrFromTypeErr err = HError ("Unhandled Type Error" : lines (show err)) [] [] []


pathFromName = pathFromRange.getNameRange



-}