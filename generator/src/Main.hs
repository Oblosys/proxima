module Main where


import System

import TypesUtils
import Parser
import qualified Gen_DocTypes
import qualified Gen_DocUtils


import List
import GenCommon
import GenParser
import GenAG
import GenEditable
import GenProxParser


--- All lines containing a --- have been altered by Martijn.
--- For changed or added functions, a --- has been put in front of it rather than on each line


--- Hole and ParseErr are still a bit hacky. Rather than being added to the datatype, they
--- are added as strings by each of the generator modules. A change on them is therefore
--- hard to realize


--- more abstraction on the types is necessary, for example simply have a function for printing
--- the ag type, and one for the Haskell type. Now inits appear everywhere in the source.




{- Generation
TODO:
-can we get rid of parse err and hole for node type? (also change rankNode and pathNode
  old generator only made nodes for holes, not parse errs
-add some static checks: double types, duplicate fieldnames (or maybe use suffix 1, 2 .. to create unique names)
-rename <constructor>Node to Node_<constructor>

-- get rid of hacks: (drop 5) (drop 9) to get list type name  and ("ParseErr" `isPrefixOf`)

-}


{-
main =
 do { docType <- parseDocumentType "DocumentType.prx"
    ; generateFile "." "Test.hs" $ Gen_DocUtils.generate docType
    ; getChar
    }
-}

generateFile :: String -> String -> [String] -> IO ()
generateFile path fileName generatedLines =
 do { let filePath = path ++ "/" ++ fileName
    ; oldContents <- readFile filePath
    ; seq (length oldContents) $ return ()
    ; case removeGeneratedContent oldContents of
        Nothing -> stop ("File "++filePath++" should contain the following line:\n\n"++delimiterLine)
        Just nonGenerated -> -- putStr $ nonGenerated ++ unlines (delimiterLine : generatedLines)
                             writeFile filePath $ nonGenerated ++ unlines (delimiterLine : generatedLines)
    } `catch` \err -> stop (show err)

removeGeneratedContent :: String -> Maybe String
removeGeneratedContent content = 
  let contentLines = lines content
  in  if any (isPrefixOf defaultLimit) contentLines
      then Just $ unlines $ takeWhile (not . isPrefixOf delimiterLine) contentLines
      else Nothing


main =
 do { args <- getArgs
    ; case args of
        [srcPath, fname] -> generateFiles srcPath fname
        _                -> 
          stop "Usage: generate <path to proxima src directory> <document type definition>.prx"
                           
    }




generateFiles srcPath fname  
     = do putStr $ "Parsing File: "++(show fname)++" ..."
          parsedFile <- parseDataTypesFile fname       -- What if the parser goes wrong?? 
          putStr $ " done\n"                           --- simply terminate with a parse error.
--          generate (srcPath++"/DocTypes_Generated.hs")         genDocumentTypes   parsedFile
          docType <- parseDocumentType fname
          generateFile srcPath "DocTypes_Generated.hs" $ Gen_DocTypes.generate docType
          generateFile srcPath "DocUtils_Generated.hs" $ Gen_DocUtils.generate docType
          generate (srcPath++"/DocumentEdit_Generated.hs")     genDocumentEdit    parsedFile
--          generate (srcPath++"/DocUtils_Generated.hs")         genDocUtils        parsedFile
          generate (srcPath++"/PresentationAG_Generated.ag") genPresentationAG  parsedFile
          generate (srcPath++"/ProxParser_Generated.hs")     genProxParser      parsedFile

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
