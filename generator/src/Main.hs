module Main where

import TypesUtils
import Parser
import qualified Gen_DocTypes
import qualified Gen_DocUtils
import qualified Gen_ProxParser
import qualified Gen_PresentationAG
import qualified Gen_DocumentEdit

import System
import List


{- Generation
TODO:

- do a diff on generated files to check that they are correct. ag and docedit have been checked already
- rename reusen to genericReuse (after diffing)
- rename <constructor>Node to Node_<constructor>
- can we get rid of parse err and hole for node type? (also change rankNode and pathNode
  old generator only made nodes for holes, not parse errs
- add some static checks: double types, duplicate fieldnames (or maybe use suffix 1, 2 .. to create unique names)
- put editable instances for prims in generator, maybe also for other modules?

- make a mechanism to add fragments from a hs file? 
-  (so the non-generated part can contain only user specified stuff) useful for editable instances for prim types
-}

main =
 do { args <- getArgs
    ; case args of
        [srcPath, fname] -> generateFiles srcPath fname
        _                -> 
          stop "Usage: generate <path to proxima instance dir> <document type definition>.prx"                           
    }
    
generateFiles srcPath fileName =
 do { docType <- parseDocumentType fileName
    ; generateFile srcPath "DocTypes_Generated.hs"       $ Gen_DocTypes.generate docType
    ; generateFile srcPath "DocUtils_Generated.hs"       $ Gen_DocUtils.generate docType
    ; generateFile srcPath "ProxParser_Generated.hs"     $ Gen_ProxParser.generate docType
    ; generateFile srcPath "PresentationAG_Generated.ag" $ Gen_PresentationAG.generate docType
    ; generateFile srcPath "DocumentEdit_Generated.hs"   $ Gen_DocumentEdit.generate docType
    }
    
generateFile :: String -> String -> [String] -> IO ()
generateFile path fileName generatedLines =
 do { putStrLn "Generating fileName"
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
