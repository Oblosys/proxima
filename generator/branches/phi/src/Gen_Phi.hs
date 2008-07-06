module Gen_Phi where

import List
import TypesUtils

import Char (toUpper)

import Language.Haskell.Parser
import Language.Haskell.Syntax

import System.IO.Unsafe

{-
 - What I want:
 - v * generate default presentations (pres.empty), at least temporary (AG)
 -   * generate editable instances (at least for type syns?)
 -   * generate show (read?) instances (might be dummy)
 -}


generate :: DocumentType -> [String]
generate decls = 
               genShows (allExternalTypes decls)
{-  where allUndeclaredTypes = getUndeclaredTypes decls
        allExternalTypes = allUndeclaredTypes \\ allTypeSyns
        allTypeSyns = filter (\x -> typeName x `elem` blup) allUndeclaredTypes
          where tpes = unsafePerformIO parseTypeSynonyms
                blup = map fst $ internalTypeSynonyms tpes
-}

allExternalTypes :: DocumentType -> [Type]
allExternalTypes decls = (allUndeclaredTypes decls) \\ allTypeSyns
  where
    allTypeSyns = filter (\x -> typeName x `elem` blup) (allUndeclaredTypes decls)
      where tpes = unsafePerformIO parseTypeSynonyms
            blup = map fst $ internalTypeSynonyms tpes
allUndeclaredTypes decls = filter (\x -> not (isDeclaredOrPrimType decls x || isListType x))
                             (getAllUsedTypes decls)

parse :: String -> HsModule
parse syntax = 
  case parseModule syntax of 
    (ParseOk ast)          -> ast
    (ParseFailed loc msg)  -> error $ srcFilename loc ++ ":" ++ show (srcLine loc) ++ ":" ++ show (srcColumn loc) ++ " " ++ msg
    
parseTypeSynonyms = do content <- readFile "gen/HsSynConvertedTypes.hs"
                       return $ parse content

internalTypeSynonyms :: HsModule -> [(String,String)]
internalTypeSynonyms (HsModule _ _ _ _ decls) = [ (n, t) | HsTypeDecl _ (HsIdent n) _ (HsTyCon (UnQual (HsIdent t))) <- decls ]


externalTypeSynonyms = ["Kind","PostTcType"] -- ["CLabelString","DeprecTxt","RuleName","TyVar","Char","Integer","Rational"] 








--------------- SHOWS -----------------------

-- for GHC
definedShows = ["Id","HsDoc_RdrName","Char","Integer","Rational","DeprecTxt","ExtraState","RdrName","HsWrapper","Type","Name", "Range","Names", "Fixity"]

genShows tpes = genBanner "Show instances" $
  concatMap genShowTpe tpes

genShowTpe tpe = 
 if typeName tpe `elem` (externalTypeSynonyms ++ definedShows)
 then [""]
 else
   ["instance Show %1 where"
   ,"  show _ = \"*external: %1*\""
   ] <~ [typeName tpe]





-------------- DEFAULT PRES ------------------------


generateDefaultPres :: DocumentType -> [String]
generateDefaultPres docType =  
  genSem $ addConsListDecls $ addListDecls $ removeDocumentDecl $ addEnrichedDocDecl docType

genSem decls = genBanner "Sem functions for default presentation" $
  concatMap genSemDecl decls
 where genSemDecl decl@(Decl (LHSBasicType _) _)    = genSemBasicDecl decls decl
       genSemDecl decl@(Decl (LHSListType _) _)     = genSemListDecl decl
       genSemDecl _ = [""]


genSemBasicDecl decls (Decl (LHSBasicType typeNm) prods) = 
  "SEM %1" <~ [typeNm] : concatMap genSemPresProd prods
    where 
       genSemPresProd (Prod _ cnstrName idpFields fields) =
         [ "  | %1 {- " ++ concatMap (flip (++) " | " . myFieldName) fields ++ "-}"
         , "      loc.pres' = (empty, [], @lhs.whitespaceMapCreated, @lhs.tokStr)"
         ] <~ [cnstrName]
       myFieldName fld = fldName ++ (if (fldTpNm `isPrefixOf` fldName')
                                     then ""
                                     else " : " ++ fldTpNm)
                           ++
                         if not (fldTpNm `elem` fieldNames)
                         then " (ext)"
                         else ""
                     where fldName@(hf:tf) = fieldName fld
                           fldName' = toUpper hf : tf
                           fldTpNm = typeName . fieldType $ fld
       fieldNames = map (lhsTypeName . declLHSType) decls ++ primTypeNames
       
              
genSemListDecl (Decl (LHSListType typeName) _) = 
  [ "SEM List_%1"
  , "  | List_%1"
  , "      loc.pres' = (row @elts.press, [], @elts.whitespaceMapCreated, @elts.tokStr)"
  ] <~ [typeName]





------------------ EDITABLES ---------------------------


generateEditables :: DocumentType -> [String]
generateEditables decls = genEditable (allExternalTypes decls)


genEditable tpes = genBanner "Editable instances" $
  concatMap genEditableTpe tpes

genEditableTpe tpe = 
 if typeName tpe `elem` externalTypeSynonyms then [""]
 else 
  ["instance Editable %1 Document Node ClipDoc UserToken where"
  ,"  hole = undefined"
  ,"  isList _ = False"
  ,"  insertList _ _ _ = Clip_Nothing"
  ,"  removeList _ _ = Clip_Nothing"
  ,"  select = undefined"
  ,"  paste = undefined"
  ,"  alternatives = undefined"
  ,"  arity = undefined"
  ,"  toClip = undefined"
  ,"  fromClip = undefined"
  ,"  parseErr = undefined"
  ,"  holeNodeConstr = undefined"
  ,"-- " ++ show tpe
  ] <~  [typeName tpe]

