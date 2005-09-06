{-
no Lexing            done by Proxima
no parsing           done by Proxima
no imports           not supported by Proxima's subset of Helium

no ResolveOperators  Proxima does not fully support operators yet
no typing strategies not that important for the editor

no desugaring        compile only until type checking
no code generation   compile only until type checking
-}

-- Daan: lvm export Set problem with ghci
-- in order to get lvm files ["Prelude", "PreludePrim", "HeliumLang", "LvmLang", "LvmIO", "LvmException"]
-- use helium on HeliumNt/lib/Prelude.hs for Prelude.lvm and coreasm on .core sources for the rest

module CompileHelium (compileHelium) where

import PhaseImport           (phaseImport)
import PhaseStaticChecks     (phaseStaticChecks)
import PhaseTypingStrategies (phaseTypingStrategies)
import TypeInferencing       (proximaTypeInferencing)
 
import TypeErrors   (TypeError)
import StaticErrors (Error)
import UHA_Syntax   (Range)
import Top.Types    (TpScheme)
import Args         (Option(..))
import CompileUtils

compileHelium :: Module -> Phase (Either Error TypeError) (TypeEnvironment, [(Range, TpScheme)])
compileHelium resolvedModule =
 do mapM_ (checkExistence myLvmPath)
       ["Prelude", "PreludePrim", "HeliumLang", "LvmLang", "LvmIO", "LvmException"]
    compile "Main" [NoLogging, Overloading] myLvmPath resolvedModule

compile :: 
   String -> [Option] -> [String] -> Module -> 
   Phase (Either Error TypeError) (TypeEnvironment, [(Range, TpScheme)])

compile fullName options lvmPath resolvedModule =
   do
       putStrLn ("Compiling " ++ fullName)

       -- Phase 3: Importing
       (_, importEnvs) <-
           phaseImport fullName resolvedModule lvmPath options
       
       -- Phase 5: Static checking 
       phaseStaticChecks fullName resolvedModule importEnvs options
          ===> \(localEnv, typeSignatures, staticWarnings) -> do
                  
          -- Phase 7: Type Inference Directives
          let combinedEnv = foldr combineImportEnvironments localEnv importEnvs
          (beforeTypeInferEnv, _) <-
             phaseTypingStrategies fullName combinedEnv typeSignatures options
          
          -- Phase 8: Type inferencing
          phaseProximaTypeInferencer resolvedModule beforeTypeInferEnv options
      
-----------------------------------
-- Type inference phase for Proxima
      
phaseProximaTypeInferencer :: 
    Module -> ImportEnvironment -> [Option] -> 
    Phase TypeError (TypeEnvironment, [(Range, TpScheme)])

phaseProximaTypeInferencer module_ completeEnv options = do
    enterNewPhase "Type inferencing" options

    -- 'W' and 'M' are predefined type inference algorithms
    let newOptions = (if AlgorithmW `elem` options
                        then filter (/= NoSpreading) . ([TreeWalkInorderTopLastPost, SolverGreedy]++) 
                        else id)
                   . (if AlgorithmM `elem` options
                        then filter (/= NoSpreading) . ([TreeWalkInorderTopFirstPre, SolverGreedy]++)  
                        else id)
                   $ options
                   
        (typeErrors, warnings, toplevelTypes, allTypes) =
           proximaTypeInferencing newOptions completeEnv module_

    case typeErrors of 
       _:_ -> return (Left typeErrors)
       []  -> return (Right (toplevelTypes, allTypes))

myLvmPath :: [String]
myLvmPath = ["lvm"] 