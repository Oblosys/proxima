module CompileHelium (compileHelium) where

-- Arjan: ProximaTypeInferencing.ag in helium cvs
-- Daan: lvm export Set problem with ghci
-- Bastiaan: CompileHelium:No substitution in case of type error
-- Arjan: overloading? 1+x :: b? 

-- in order to get lvm files ["Prelude", "PreludePrim", "HeliumLang", "LvmLang", "LvmIO", "LvmException"]
-- use helium on HeliumNt/lib/Prelude.hs for Prelude.lvm and coreasm on .core sources for the rest


import PhaseImport

--module Main
import Maybe(fromJust, isNothing)
import Standard(searchPathMaybe,getLvmPath, splitPath)
--module Compile
import CompileUtils
import Utils
import IOExts(writeIORef)

--module PhaseResolveOperators(phaseResolveOperators) where
import CompileUtils
import ResolveOperators(resolveOperators, operatorsFromModule)
import qualified PrettyPrinting(sem_Module)
import FiniteMap
--module PhaseStaticChecks(phaseStaticChecks) where
import CompileUtils
import Warnings(Warning)
import StaticErrors(errorsLogCode)
import qualified StaticChecks(sem_Module)
--module PhaseTypeInferencer (phaseTypeInferencer) where
import CompileUtils
import Tree(flattenM, flattenW)
import Warnings(Warning)
import qualified ProximaTypeInferencing(sem_Module)

import Types -- temporary
import FiniteMap
import UHA_Utils
import UHA_Syntax
----


import StaticErrors(Error, Errors)
import TypeErrors(TypeError, TypeErrors)
import Args

{-

no Lexing            done by Proxima
no parsing           done by Proxima
no imports           not supported by Proxima's subset of Helium

no ResolveOperators  Proxima does not fully support operators yet
no typing strategies not that important for the editor

no desugaring        compile only until type checking
no code generation   compile only until type checking

Compile:

--import PhaseLexer
--import PhaseParser
import PhaseImport   -- not inlined, since it cannot exit. It is imported normally by CompileHelium
--import PhaseResolveOperators
import PhaseStaticChecks
--import PhaseTypingStrategies
import PhaseTypeInferencer
--import PhaseDesugarer
--import PhaseCodeGenerator

The contents of the imported modules are inlined in this module. Instead of terminating on error
each now returns its errors and warnings.
-}





lvmPath = ["."]

--                                              type env       substitution        top level env
compileHelium :: Module -> IO (Either [Error] ([TypeError], (TypeEnvironment, WrappedSubstitution, [(Range, Tp)])))
compileHelium mod =
 do {  mapM (checkExistence lvmPath) 
        ["Prelude", "PreludePrim", "HeliumLang", "LvmLang", "LvmIO", "LvmException"]
    ; compile "Henk" [NoLogging] lvmPath [] mod
    }


---------module Main

-- compiler is not invoked, so .hs will not do. We need the .lvm
resolve :: [String] -> String -> IO (Maybe String)
resolve path name = 
   -- do maybeFullName <- searchPathMaybe path ".hs" name
   --    case maybeFullName of
   --        Just fullName -> return (Just fullName)
   --        Nothing       -> 
           searchPathMaybe path ".lvm" name


checkExistence :: [String] -> String -> IO ()
checkExistence path name =
    do
        maybeLocation <- resolve path name
        when (isNothing maybeLocation) $ do
            putStr
                (  "Cannot find "
                ++ name
                ++ ".lvm in search path:\n"
                ++ unlines (map ("\t" ++) path)
                -- ++ "See the installation manual on setting the environment variable LVMPATH\n"
                )
            exitWith (ExitFailure 1)

                                           


compile :: String -> [Option] -> [String] -> [String] -> Module -> IO (Either [Error] ([TypeError], (TypeEnvironment, WrappedSubstitution, [(Range, Tp)])))
compile fullName options lvmPath doneModules mod =
    do
        putStrLn ("Compiling " ++ fullName)
{-
        -- Store the current module file-name and its context in
        -- two IO refs (unsafe! only used for internal error bug-report)
        writeIORef refToCurrentFileName fullName
        writeIORef refToCurrentImported doneModules

        contents <- safeReadFile fullName

        -- Phase 1: Lexing
        (lexerWarnings, tokens) <- 
            phaseLexer fullName doneModules contents options
                        
        unless (NoWarnings `elem` options) $
            showMessages lexerWarnings

        -- Phase 2: Parsing
        parsedModule <- 
            phaseParser fullName doneModules tokens options
-}
        let parsedModule = mod
        
        -- Phase 3: Importing
        (indirectionDecls, importEnvs) <-
            phaseImport fullName parsedModule lvmPath options
      
        
        -- Phase 4: Resolving operators
{-        resolvedModule <- 
            phaseResolveOperators fullName doneModules parsedModule importEnvs options
-}
        let resolvedModule = parsedModule
                    
--        stopCompilingIf (StopAfterParser `elem` options)

        -- Phase 5: Static checking
        phase5 <- phaseStaticChecks fullName doneModules resolvedModule importEnvs options   
        case phase5 of
          Left staticErrs -> return (Left staticErrs)
          Right (localEnv, staticWarnings) ->
           do
--        unless (NoWarnings `elem` options) $
--            showMessages staticWarnings

--        stopCompilingIf (StopAfterStaticAnalysis `elem` options)

        
{-        -- Phase 6: Typing Strategies
        (completeEnv, typingStrategiesDecls) <-
            phaseTypingStrategies fullName localEnv importEnvs options
-}
            let completeEnv = localEnv
         
            -- Phase 7: Type inferencing
            phase7 <- phaseTypeInferencer fullName resolvedModule doneModules localEnv 
                                          completeEnv options
            case phase7 of
              Left typeErrs -> return $ Right ( typeErrs, (emptyFM, error "CompileHelium:No substitution in case of type error", []))
              Right (subst, alltypes, finalEnv, inferredTypes, overloadedVars, toplevelTypes, typeWarnings) ->
               do return $ Right ([], (toplevelTypes, subst, alltypes))
               
               
               
    --        unless (NoWarnings `elem` options) $
    --            showMessages typeWarnings
    {-
            stopCompilingIf (StopAfterTypeInferencing `elem` options)
    
            -- Phase 8: Desugaring
            coreModule <-                
                phaseDesugarer fullName resolvedModule 
                                (typingStrategiesDecls ++ indirectionDecls) 
                                finalEnv inferredTypes overloadedVars toplevelTypes options
    
            stopCompilingIf (StopAfterDesugar `elem` options)
    
            -- Phase 9: Code generation
            phaseCodeGenerator fullName coreModule options
            
            unless (NoLogging `elem` options) $ 
                sendLog "C" fullName doneModules options
    
            let number = length staticWarnings + length typeWarnings + length lexerWarnings
            putStrLn $ "Compilation successful" ++
                          if number == 0 || (NoWarnings `elem` options)
                            then ""
                            else " with " ++ show number ++ " warning" ++ if number == 1 then "" else "s"
-}


safeReadFile :: String -> IO String
safeReadFile fullName = 
    catch 
        (readFile fullName)
        (\ioError -> 
            let message = "Unable to read file " ++ show fullName 
                       ++ " (" ++ show ioError ++ ")"
            in throw message)

stopCompilingIf :: Bool -> IO ()
stopCompilingIf bool = when bool (exitWith (ExitFailure 1))




---------module PhaseResolveOperators(phaseResolveOperators) where

phaseResolveOperators :: String -> [String] -> Module -> [ImportEnvironment] -> 
                            [Option] -> IO Module
phaseResolveOperators fullName doneModules moduleBeforeResolve importEnvs options = do
    enterNewPhase "Resolving operators" options

    let importOperatorTable = 
            foldr1 plusFM ( operatorsFromModule moduleBeforeResolve
                          : map operatorTable importEnvs
                          )
        (module_, resolveErrors) = 
                  resolveOperators importOperatorTable moduleBeforeResolve

    when (not (null resolveErrors)) $ do
        unless (NoLogging `elem` options) $ 
            sendLog "R" fullName doneModules options
        showErrorsAndExit resolveErrors 20 options

    when (DumpUHA `elem` options) $
        putStrLn $ show $ PrettyPrinting.sem_Module module_
    
    return module_



---------module PhaseStaticChecks(phaseStaticChecks) where


phaseStaticChecks :: String -> [String] -> Module -> [ImportEnvironment] -> 
                        [Option] -> IO (Either Errors (ImportEnvironment, [Warning]))
phaseStaticChecks fullName doneModules module_ importEnvs options = do
    enterNewPhase "Static checking" options

    let (_, baseName, _) = splitFilePath fullName

        (localEnv, errors, _, warnings) =
            StaticChecks.sem_Module module_ baseName importEnvs

{-    when (not (null errors)) $ do
        when (DumpInformationForAllModules `elem` options) $
            putStrLn (show (foldr combineImportEnvironments 
                emptyEnvironment importEnvs))
        unless (NoLogging `elem` options) $ 
            sendLog ("S"++errorsLogCode errors) fullName doneModules options
        showErrorsAndExit errors 20 options
  -}  
    if (null errors) then return $ Right (localEnv, warnings)
                     else return $ Left errors

---------module PhaseTypeInferencer (phaseTypeInferencer) where


phaseTypeInferencer :: 
    String -> Module -> [String] -> ImportEnvironment ->
        ImportEnvironment -> [Option] -> 
           IO (Either TypeErrors
                     (WrappedSubstitution, [(Range, Tp)],
                      ImportEnvironment, FiniteMap NameWithRange TpScheme  {- == LocalTypes -}, 
                      FiniteMap NameWithRange (NameWithRange, QType) {- OverloadedVariables -}, TypeEnvironment, [Warning]))
phaseTypeInferencer fullName module_ doneModules localEnv completeEnv options = do
    enterNewPhase "Type inferencing" options
                                              -- prox                         prox
    let (debugIO, localTypes, overloadedVars, _, substitution, toplevelTypes, alltypes, typeErrors, warnings) =
            ProximaTypeInferencing.sem_Module module_
                completeEnv
                options        
        
        -- add the top-level types (including the inferred types)
        finalEnv = addToTypeEnvironment toplevelTypes completeEnv
        inferredTypes = addListToFM localTypes 
                [ (NameWithRange name, ts) | (name, ts) <- fmToList (typeEnvironment finalEnv) ]
    
    when (DumpTypeDebug `elem` options) debugIO  
    
{-
    putStrLn (unlines ("" : "toplevelTypes: " : map (\(n,ts) -> show (NameWithRange n) ++ " :: "++show (getQualifiedType ts)) (fmToList toplevelTypes)))
    putStrLn (unlines ("" : "localTypes:" : map show (fmToList localTypes)))
    putStrLn (unlines ("" : "overloadedVars:"   : map (\(n,(m,t)) -> show n ++ " in scope of " ++ show m ++" has type " ++ show t) (fmToList overloadedVars)))        
-}
{-
    when (not (null typeErrors)) $ do
        when (DumpInformationForAllModules `elem` options) $
            putStr (show completeEnv)
        unless (NoLogging `elem` options) $ 
            sendLog "T" fullName doneModules options
        showErrorsAndExit (reverse typeErrors) maximumNumberOfTypeErrors options
-}
    -- Dump information
    if (DumpInformationForAllModules `elem` options)
      then
         putStrLn (show finalEnv)
      else if (DumpInformationForThisModule `elem` options)
             then
                putStrLn (show (addToTypeEnvironment toplevelTypes localEnv))
             else
                return ()
    if (null typeErrors) then return $ Right (substitution, alltypes, finalEnv, inferredTypes, overloadedVars, toplevelTypes, warnings)
                         else return $ Left typeErrors 



maximumNumberOfTypeErrors :: Int
maximumNumberOfTypeErrors = 3






-- Martijn: function to extract name from compiled program
getModuleName m@(Module_Module moduleRange maybeName exports 
                    (Body_Body bodyRange explicitImportDecls decls)) =
    case maybeName of
        MaybeName_Just n -> getNameName n
        _ -> "Main"                        -- no module name? Does that ever happen?
