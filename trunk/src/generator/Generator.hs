module Main where

import System

import GenCommon
import GenParser
import GenAG
import GenTypes
import GenEditable
import GenDocUtils
import GenProxParser 

--- All lines containing a --- have been altered by Martijn.
--- For changed or added functions, a --- has been put in front of it rather than on each line


--- Hole and ParseErr are still a bit hacky. Rather than being added to the datatype, they
--- are added as strings by each of the generator modules. A change on them is therefore
--- hard to realize


--- more abstraction on the types is necessary, for example simply have a function for printing
--- the ag type, and one for the Haskell type. Now inits appear everywhere in the source.


--- use the name of the generated module for the module name, eg. GenDocumentEdit instead of GenEditable


---------------------------------------------------------------------
--       M A I N                                                   --
---------------------------------------------------------------------

main =
  getArgs >>= \args->
  if length args /= 1 then
    putStrLn "Usage: generate file" >>
    exitWith (ExitFailure 1)
  else
    generateFiles (head args)


generateFiles fname  
     = do putStr $ "Parsing File: "++(show fname)++" ..."
          parsedFile <- parseDataTypesFile fname       -- What if the parser goes wrong?? 
          putStr $ " done\n"                           --- simply terminate with a parse error.
          generate "../evaluation/DocTypes_Generated.hs"         genDocumentTypes   parsedFile
          generate "../evaluation/DocumentEdit_Generated.hs"     genDocumentEdit    parsedFile
          generate "../evaluation/DocUtils_Generated.hs"         genDocUtils        parsedFile
          generate "../presentation/PresentationAG_Generated.ag" genPresentationAG  parsedFile
          generate "../presentation/ProxParser_Generated.hs"     genProxParser      parsedFile

-- make this function more clear
generate filename func parsedFile
     = do includeText <- readFile filename 
          putStr $ "Generating "++filename++"..."  
          let includeTextLines = lines includeText
          if  elem defaultLimit includeTextLines
              then do let includeTypes = takeWhile (/= defaultLimit) (lines  includeText)
                      length includeTypes `seq` (writeFile (filename) . unlines . (func includeTypes)) parsedFile -- Reading file !!!
                      putStr $ " done\n"
              else do putStr $ " failed - Couldn't find: " ++ defaultLimit ++ "\n"
          
