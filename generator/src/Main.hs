module Main where

import TypesUtils
import Parser
import qualified Gen_DocTypes
import qualified Gen_DocUtils
import qualified Gen_ProxParser
import qualified Gen_PresentationAG

import System
import List
import GenCommon
import GenParser
import GenAG
import GenEditable


{- Generation
TODO:
-can we get rid of parse err and hole for node type? (also change rankNode and pathNode
  old generator only made nodes for holes, not parse errs
-add some static checks: double types, duplicate fieldnames (or maybe use suffix 1, 2 .. to create unique names)
-rename <constructor>Node to Node_<constructor>

-- get rid of hacks: (drop 5) (drop 9) to get list type name
-- do a diff on generated files to check that they are correct.
-- rename reusen to genericReuse (after diffing)
-- make a mechanism to add fragments from a hs file? (so the non-generated part can contain only user specified stuff)
-}



main =
 do { docType <- parseDocumentType "../DocumentType.prx"
    ; output <- generateFile ".." "PresentationAG_Generated.ag" $ Gen_PresentationAG.generate docType
    ; putStr output
    ; getChar
    }


generateFile :: String -> String -> [String] -> IO String
generateFile path fileName generatedLines =
 do { let filePath = path ++ "/" ++ fileName
    ; oldContents <- readFile filePath
    ; seq (length oldContents) $ return ()
    ; case removeGeneratedContent oldContents of
        Nothing -> stop ("File "++filePath++" should contain the following line:\n\n"++delimiterLine)
        Just nonGenerated -> do { writeFile filePath $ nonGenerated ++ unlines (delimiterLine : generatedLines)
                                ; return $ nonGenerated ++ unlines (delimiterLine : generatedLines)
                                }
    } `catch` \err -> stop (show err)

removeGeneratedContent :: String -> Maybe String
removeGeneratedContent content = 
  let contentLines = lines content
  in  if any (isPrefixOf defaultLimit) contentLines
      then Just $ unlines $ takeWhile (not . isPrefixOf delimiterLine) contentLines
      else Nothing

{-
main =
 do { args <- getArgs
    ; case args of
        [srcPath, fname] -> generateFiles srcPath fname
        _                -> 
          stop "Usage: generate <path to proxima instance dir> <document type definition>.prx"
                           
    }
-}



generateFiles srcPath fname  
     = do putStr $ "Parsing File: "++(show fname)++" ..."
          parsedFile <- parseDataTypesFile fname       -- What if the parser goes wrong?? 
          putStr $ " done\n"                           --- simply terminate with a parse error.
--          generate (srcPath++"/DocTypes_Generated.hs")         genDocumentTypes   parsedFile
          docType <- parseDocumentType fname
          generateFile srcPath "DocTypes_Generated.hs" $ Gen_DocTypes.generate docType
          generateFile srcPath "DocUtils_Generated.hs" $ Gen_DocUtils.generate docType
          generateFile srcPath "ProxParser_Generated.hs" $ Gen_ProxParser.generate docType
          generate (srcPath++"/DocumentEdit_Generated.hs")     genDocumentEdit    parsedFile
--          generate (srcPath++"/DocUtils_Generated.hs")         genDocUtils        parsedFile
          generate (srcPath++"/PresentationAG_Generated.ag") genPresentationAG  parsedFile
--          generate (srcPath++"/ProxParser_Generated.hs")     genProxParser      parsedFile

-- make this function more clear
generate filename func parsedFile
     = do includeText <- readFile filename 
          seq (length includeText) $ return ()
          putStr $ "Generating "++filename++"..."  
          let includeTextLines = lines includeText
          if  any (isPrefixOf defaultLimit) includeTextLines
              then do let includeTypes = takeWhile (not . isPrefixOf defaultLimit) (lines  includeText)
                      length includeTypes `seq` (writeFile (filename) . unlines . (func includeTypes)) parsedFile -- Reading file !!!
                      putStr $ " done\n"
              else do putStr $ " vailed - Couldn't find: " ++ defaultLimit ++ "\n"
                      exitWith (ExitFailure 1)
