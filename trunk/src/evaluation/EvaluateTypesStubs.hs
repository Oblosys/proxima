module EvaluateTypesStubs where

-- copy of EvaluateTypes that does not import the Helium modules

import DocTypes
import DocTypes_Generated

import DebugLevels
import IOExts -- evaluate has IO, so the unsafePerformIO is only temporary

--import CompileHelium

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

range :: [Int] -> Range
range pth = Range_Range (Position_Position (show pth) (-1) (-1)) Position_Unknown



--Helium stub
henk2 :: ([HeliumMessage], [(PathDoc,String)], [(String,String)])
henk2 = ([HError [ "Type checking not available, because Helium Compiler is not imported.",
                     "In EvalPresent, import EvaluateTypes instead of EvaluateTypeStubs."] [] [] []
           ], [], [])


evaluate :: Document -> ([HeliumMessage], [(PathDoc,String)], [(String,String)])
evaluate doc = henk2 

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