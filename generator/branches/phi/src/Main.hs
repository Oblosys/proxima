module Main where

import TypesUtils
import Parser
import qualified Gen_DocTypes
import qualified Gen_DocUtils
import qualified Gen_ProxParser
import qualified Gen_PresentationAG
import qualified Gen_DocumentEdit
import qualified Gen_Phi --gerbo

import System
import List

{-

TODO:
- add some static checks: double types, duplicate fieldnames (or maybe use suffix 1, 2 .. to create unique names)
- put all standard code in the generator (e.g. editable instances for prim types)
  This way the non-generated part consists only of user-specified code.

- make a record with all variations of declarations (with lists, with parse errs, etc.)
    general functions like getDeclaredTypeNames can get the record and therefore do not depend on
    what kind of decls are passed to it.
-}

main =
 do { args <- getArgs
    ; case args of
        [srcPath, fname] -> generateFiles srcPath fname False
        [srcPath, fname, flag] -> generateFiles srcPath fname True -- gerbo
        _                -> 
          stop "Usage: generate <path to proxima instance dir> <document type definition>.prx"                           
    }
    
generateFiles srcPath fileName genPhi =
 do { docType <- parseDocumentType fileName
    ; generateFile srcPath "DocTypes_Generated.hs"       $ Gen_DocTypes.generate docType
    ; generateFile srcPath "DocUtils_Generated.hs"       $ Gen_DocUtils.generate docType
    ; generateFile srcPath "ProxParser_Generated.hs"     $ Gen_ProxParser.generate docType
    ; generateFile srcPath "PresentationAG_Generated.ag" $ Gen_PresentationAG.generate docType
    ; generateFile srcPath "DocumentEdit_Generated.hs"   $ Gen_DocumentEdit.generate docType
    ; if genPhi
      then do generateFile srcPath "Phi_Generated.hs" $ Gen_Phi.generate docType 
              generateFile srcPath "PhiPresentationAG-template.ag" $ Gen_Phi.generateDefaultPres docType
              generateFile srcPath "Phi_Editables.hs" $ Gen_Phi.generateEditables docType
      else return ()
    }
    
generateFile :: String -> String -> [String] -> IO ()
generateFile path fileName generatedLines =
 do { putStrLn $ "Generating "++fileName
    ; let filePath = path ++ "/" ++ fileName
    ; oldContents <- readFile filePath
    ; seq (length oldContents) $ return ()
    ; case removeGeneratedContent oldContents of
        Nothing -> stop ("File "++filePath++" should contain the following line:\n\n"++delimiterLine)
        Just nonGenerated -> writeFile filePath $ nonGenerated ++ unlines (delimiterLine : generatedLines)
    } `catch` \err -> stop (show err)

removeGeneratedContent :: String -> Maybe String
removeGeneratedContent content = 
  let contentLines = lines content
  in  if any (isPrefixOf delimiterLine) contentLines
      then Just $ unlines $ takeWhile (not . isPrefixOf delimiterLine) contentLines
      else Nothing
