{-# OPTIONS_GHC -fbang-patterns #-}

-- UUAGC 0.9.10 (PrintCode.ag)
module PrintCode where

import Char (isAlphaNum)
import Pretty
import Code
import Patterns
import Options
import CommonTypes (attrname, _LOC, getName, nullIdent)
import Data.List(intersperse)
import System.IO
import System.Directory
import CommonTypes(BlockInfo, BlockType(..), identifier)


import Pretty
import Patterns
import Data.List(partition)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map


-- Patterns.ag imports
import UU.Scanner.Position(Pos)
import CommonTypes (ConstructorIdent,Identifier)

type PP_Docs = [PP_Doc]


ppMultiSeqH :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqH = ppMultiSeq' (>#<)

ppMultiSeqV :: [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeqV = ppMultiSeq' (>-<)

ppMultiSeq' :: (PP_Doc -> PP_Doc -> PP_Doc) -> [PP_Doc] -> PP_Doc -> PP_Doc
ppMultiSeq' next strictArgs expr
  = foldr (\v r -> (v >#< "`seq`") `next` pp_parens r) expr strictArgs



reallySimple :: String -> Bool
reallySimple = and . map (\x -> isAlphaNum x || x=='_')

ppTuple True  pps = "(" >|< pp_block " " (replicate (length pps `max` 1) ')') ",(" pps
ppTuple False pps = "(" >|< pp_block " " ")" "," pps
ppUnboxedTuple True pps  = "(# " >|< pp_block " " (concat $ replicate (length pps `max` 1) " #)") ",(# " pps
ppUnboxedTuple False pps = "(# " >|< pp_block " " " #)" "," pps



locname' n = "_loc_" ++ getName n


renderDocs :: [PP_Doc] -> String
renderDocs pps = foldr (.) id (map (\d -> (disp d 50000) . ( '\n':) ) pps) ""


writeModule :: FilePath -> [PP_Doc] -> IO ()
writeModule path docs
  = do bExists <- doesFileExist path
       if bExists
        then do input <- readFile path
                seq (length input) (return ())
                if input /= output
                 then dumpIt
                 else return ()
        else dumpIt
  where
    output = renderDocs docs
    dumpIt = writeFile path output


-- Unboxed tuples
--   unbox  Whether unboxed tuples are wanted or not
--   inh    The inherited attributes. 
--          If there are none, no unboxing can take place, 
--          because in that case the semantic function (a top-level identifier) would have an unboxed type.
-- Of course we can't have an unboxed 1-tuple
mkTupleExpr :: Bool -> Bool -> Exprs -> Expr
mkTupleExpr unbox noInh exprs | not unbox || noInh || length exprs == 1 = TupleExpr exprs
                              | otherwise                               = UnboxedTupleExpr exprs
mkTupleType :: Bool -> Bool -> Types -> Type
mkTupleType unbox noInh tps | not unbox || noInh || length tps == 1 = TupleType tps
                            | otherwise                             = UnboxedTupleType tps
mkTupleLhs :: Bool -> Bool -> [String] -> Lhs
mkTupleLhs  unbox noInh comps | not unbox || noInh || length comps == 1 = TupleLhs comps
                              | otherwise                               = UnboxedTupleLhs comps
-- CaseAlt -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative CaseAlt:
         child left           : Lhs 
         child expr           : Expr 
-}
-- cata
sem_CaseAlt :: CaseAlt  ->
               T_CaseAlt 
sem_CaseAlt !(CaseAlt _left _expr )  =
    (sem_CaseAlt_CaseAlt (sem_Lhs _left ) (sem_Expr _expr ) )
-- semantic domain
newtype T_CaseAlt  = T_CaseAlt (Bool ->
                                Options ->
                                String ->
                                ( PP_Docs))
data Inh_CaseAlt  = Inh_CaseAlt {nested_Inh_CaseAlt :: !(Bool),options_Inh_CaseAlt :: !(Options),outputfile_Inh_CaseAlt :: !(String)}
data Syn_CaseAlt  = Syn_CaseAlt {pps_Syn_CaseAlt :: !(PP_Docs)}
wrap_CaseAlt :: T_CaseAlt  ->
                Inh_CaseAlt  ->
                Syn_CaseAlt 
wrap_CaseAlt !(T_CaseAlt sem ) !(Inh_CaseAlt _lhsInested _lhsIoptions _lhsIoutputfile )  =
    (let ( !_lhsOpps) =
             (sem _lhsInested _lhsIoptions _lhsIoutputfile )
     in  (Syn_CaseAlt _lhsOpps ))
sem_CaseAlt_CaseAlt :: T_Lhs  ->
                       T_Expr  ->
                       T_CaseAlt 
sem_CaseAlt_CaseAlt !(T_Lhs left_ ) !(T_Expr expr_ )  =
    (T_CaseAlt (\ (!_lhsInested)
                  (!_lhsIoptions)
                  (!_lhsIoutputfile) ->
                    (case (_lhsIoutputfile) of
                     { !_exprOoutputfile ->
                     (case (_lhsIoptions) of
                      { !_exprOoptions ->
                      (case (_lhsInested) of
                       { !_exprOnested ->
                       (case (_lhsIoutputfile) of
                        { !_leftOoutputfile ->
                        (case (_lhsIoptions) of
                         { !_leftOoptions ->
                         (case (_lhsInested) of
                          { !_leftOnested ->
                          (case (False) of
                           { !_leftOisDeclOfLet ->
                           (case ((expr_ _exprOnested _exprOoptions _exprOoutputfile )) of
                            { ( !_exprIpp) ->
                            (case ((left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile )) of
                             { ( !_leftIpp) ->
                             (case (["{" >#< _leftIpp >#< "->", _exprIpp >#< "}"]) of
                              { !_lhsOpps ->
                              ( _lhsOpps) }) }) }) }) }) }) }) }) }) })) )
-- CaseAlts ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : CaseAlt 
         child tl             : CaseAlts 
      alternative Nil:
-}
-- cata
sem_CaseAlts :: CaseAlts  ->
                T_CaseAlts 
sem_CaseAlts !list  =
    (Prelude.foldr sem_CaseAlts_Cons sem_CaseAlts_Nil (Prelude.map sem_CaseAlt list) )
-- semantic domain
newtype T_CaseAlts  = T_CaseAlts (Bool ->
                                  Options ->
                                  String ->
                                  ( PP_Docs))
data Inh_CaseAlts  = Inh_CaseAlts {nested_Inh_CaseAlts :: !(Bool),options_Inh_CaseAlts :: !(Options),outputfile_Inh_CaseAlts :: !(String)}
data Syn_CaseAlts  = Syn_CaseAlts {pps_Syn_CaseAlts :: !(PP_Docs)}
wrap_CaseAlts :: T_CaseAlts  ->
                 Inh_CaseAlts  ->
                 Syn_CaseAlts 
wrap_CaseAlts !(T_CaseAlts sem ) !(Inh_CaseAlts _lhsInested _lhsIoptions _lhsIoutputfile )  =
    (let ( !_lhsOpps) =
             (sem _lhsInested _lhsIoptions _lhsIoutputfile )
     in  (Syn_CaseAlts _lhsOpps ))
sem_CaseAlts_Cons :: T_CaseAlt  ->
                     T_CaseAlts  ->
                     T_CaseAlts 
sem_CaseAlts_Cons !(T_CaseAlt hd_ ) !(T_CaseAlts tl_ )  =
    (T_CaseAlts (\ (!_lhsInested)
                   (!_lhsIoptions)
                   (!_lhsIoutputfile) ->
                     (case (_lhsIoutputfile) of
                      { !_tlOoutputfile ->
                      (case (_lhsIoptions) of
                       { !_tlOoptions ->
                       (case (_lhsInested) of
                        { !_tlOnested ->
                        (case (_lhsIoutputfile) of
                         { !_hdOoutputfile ->
                         (case (_lhsIoptions) of
                          { !_hdOoptions ->
                          (case (_lhsInested) of
                           { !_hdOnested ->
                           (case ((tl_ _tlOnested _tlOoptions _tlOoutputfile )) of
                            { ( !_tlIpps) ->
                            (case ((hd_ _hdOnested _hdOoptions _hdOoutputfile )) of
                             { ( !_hdIpps) ->
                             (case (_hdIpps ++ _tlIpps) of
                              { !_lhsOpps ->
                              ( _lhsOpps) }) }) }) }) }) }) }) }) })) )
sem_CaseAlts_Nil :: T_CaseAlts 
sem_CaseAlts_Nil  =
    (T_CaseAlts (\ (!_lhsInested)
                   (!_lhsIoptions)
                   (!_lhsIoutputfile) ->
                     (case ([]) of
                      { !_lhsOpps ->
                      ( _lhsOpps) })) )
-- Chunk -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         isDeclOfLet          : Bool
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nested               : Bool
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         textBlockMap         : Map BlockInfo PP_Doc
         textBlocks           : PP_Doc
      synthesized attributes:
         appendCommon         : [[PP_Doc]]
         appendMain           : [[PP_Doc]]
         genSems              : IO ()
         imports              : [String]
         pps                  : PP_Docs
   alternatives:
      alternative Chunk:
         child name           : {String}
         child comment        : Decl 
         child info           : Decls 
         child dataDef        : Decls 
         child cataFun        : Decls 
         child semDom         : Decls 
         child semWrapper     : Decls 
         child semFunctions   : Decls 
         child semNames       : {[String]}
         visit 0:
            local outputfile  : _
            local exports     : _
-}
-- cata
sem_Chunk :: Chunk  ->
             T_Chunk 
sem_Chunk !(Chunk _name _comment _info _dataDef _cataFun _semDom _semWrapper _semFunctions _semNames )  =
    (sem_Chunk_Chunk _name (sem_Decl _comment ) (sem_Decls _info ) (sem_Decls _dataDef ) (sem_Decls _cataFun ) (sem_Decls _semDom ) (sem_Decls _semWrapper ) (sem_Decls _semFunctions ) _semNames )
-- semantic domain
newtype T_Chunk  = T_Chunk (PP_Doc ->
                            Bool ->
                            String ->
                            String ->
                            (String -> String -> String -> Bool -> String) ->
                            Bool ->
                            Options ->
                            String ->
                            String ->
                            (Map BlockInfo PP_Doc) ->
                            PP_Doc ->
                            ( ([[PP_Doc]]),([[PP_Doc]]),(IO ()),([String]),PP_Docs))
data Inh_Chunk  = Inh_Chunk {importBlocks_Inh_Chunk :: !(PP_Doc),isDeclOfLet_Inh_Chunk :: !(Bool),mainFile_Inh_Chunk :: !(String),mainName_Inh_Chunk :: !(String),moduleHeader_Inh_Chunk :: !(String -> String -> String -> Bool -> String),nested_Inh_Chunk :: !(Bool),options_Inh_Chunk :: !(Options),optionsLine_Inh_Chunk :: !(String),pragmaBlocks_Inh_Chunk :: !(String),textBlockMap_Inh_Chunk :: !(Map BlockInfo PP_Doc),textBlocks_Inh_Chunk :: !(PP_Doc)}
data Syn_Chunk  = Syn_Chunk {appendCommon_Syn_Chunk :: !([[PP_Doc]]),appendMain_Syn_Chunk :: !([[PP_Doc]]),genSems_Syn_Chunk :: !(IO ()),imports_Syn_Chunk :: !([String]),pps_Syn_Chunk :: !(PP_Docs)}
wrap_Chunk :: T_Chunk  ->
              Inh_Chunk  ->
              Syn_Chunk 
wrap_Chunk !(T_Chunk sem ) !(Inh_Chunk _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks )  =
    (let ( !_lhsOappendCommon,!_lhsOappendMain,!_lhsOgenSems,!_lhsOimports,!_lhsOpps) =
             (sem _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks )
     in  (Syn_Chunk _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps ))
sem_Chunk_Chunk :: String ->
                   T_Decl  ->
                   T_Decls  ->
                   T_Decls  ->
                   T_Decls  ->
                   T_Decls  ->
                   T_Decls  ->
                   T_Decls  ->
                   ([String]) ->
                   T_Chunk 
sem_Chunk_Chunk !name_ !(T_Decl comment_ ) !(T_Decls info_ ) !(T_Decls dataDef_ ) !(T_Decls cataFun_ ) !(T_Decls semDom_ ) !(T_Decls semWrapper_ ) !(T_Decls semFunctions_ ) !semNames_  =
    (T_Chunk (\ (!_lhsIimportBlocks)
                (!_lhsIisDeclOfLet)
                (!_lhsImainFile)
                (!_lhsImainName)
                (!_lhsImoduleHeader)
                (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoptionsLine)
                (!_lhsIpragmaBlocks)
                (!_lhsItextBlockMap)
                (!_lhsItextBlocks) ->
                  (case (if sepSemMods _lhsIoptions
                         then _lhsImainFile ++ "_" ++ name_ ++ ".hs"
                         else _lhsImainFile ++ ".hs") of
                   { !_outputfile ->
                   (case (_outputfile) of
                    { !_semDomOoutputfile ->
                    (case (_lhsIoptions) of
                     { !_semDomOoptions ->
                     (case (_lhsInested) of
                      { !_semDomOnested ->
                      (case (_lhsIisDeclOfLet) of
                       { !_semDomOisDeclOfLet ->
                       (case (_outputfile) of
                        { !_dataDefOoutputfile ->
                        (case (_lhsIoptions) of
                         { !_dataDefOoptions ->
                         (case (_lhsInested) of
                          { !_dataDefOnested ->
                          (case (_lhsIisDeclOfLet) of
                           { !_dataDefOisDeclOfLet ->
                           (case (_outputfile) of
                            { !_commentOoutputfile ->
                            (case (_lhsIoptions) of
                             { !_commentOoptions ->
                             (case (_lhsInested) of
                              { !_commentOnested ->
                              (case (_lhsIisDeclOfLet) of
                               { !_commentOisDeclOfLet ->
                               (case ((semDom_ _semDomOisDeclOfLet _semDomOnested _semDomOoptions _semDomOoutputfile )) of
                                { ( !_semDomIpps) ->
                                (case ((dataDef_ _dataDefOisDeclOfLet _dataDefOnested _dataDefOoptions _dataDefOoutputfile )) of
                                 { ( !_dataDefIpps) ->
                                 (case ((comment_ _commentOisDeclOfLet _commentOnested _commentOoptions _commentOoutputfile )) of
                                  { ( !_commentIpp) ->
                                  (case ([ [_commentIpp]
                                         , _dataDefIpps
                                         , _semDomIpps
                                         ]) of
                                   { !_lhsOappendCommon ->
                                   (case (_outputfile) of
                                    { !_semWrapperOoutputfile ->
                                    (case (_lhsIoptions) of
                                     { !_semWrapperOoptions ->
                                     (case (_lhsInested) of
                                      { !_semWrapperOnested ->
                                      (case (_lhsIisDeclOfLet) of
                                       { !_semWrapperOisDeclOfLet ->
                                       (case (_outputfile) of
                                        { !_cataFunOoutputfile ->
                                        (case (_lhsIoptions) of
                                         { !_cataFunOoptions ->
                                         (case (_lhsInested) of
                                          { !_cataFunOnested ->
                                          (case (_lhsIisDeclOfLet) of
                                           { !_cataFunOisDeclOfLet ->
                                           (case ((semWrapper_ _semWrapperOisDeclOfLet _semWrapperOnested _semWrapperOoptions _semWrapperOoutputfile )) of
                                            { ( !_semWrapperIpps) ->
                                            (case ((cataFun_ _cataFunOisDeclOfLet _cataFunOnested _cataFunOoptions _cataFunOoutputfile )) of
                                             { ( !_cataFunIpps) ->
                                             (case ([ [_commentIpp]
                                                    , _cataFunIpps
                                                    , _semWrapperIpps
                                                    ]) of
                                              { !_lhsOappendMain ->
                                              (case (_outputfile) of
                                               { !_semFunctionsOoutputfile ->
                                               (case (_lhsIoptions) of
                                                { !_semFunctionsOoptions ->
                                                (case (_lhsInested) of
                                                 { !_semFunctionsOnested ->
                                                 (case (_lhsIisDeclOfLet) of
                                                  { !_semFunctionsOisDeclOfLet ->
                                                  (case (_outputfile) of
                                                   { !_infoOoutputfile ->
                                                   (case (_lhsIoptions) of
                                                    { !_infoOoptions ->
                                                    (case (_lhsInested) of
                                                     { !_infoOnested ->
                                                     (case (_lhsIisDeclOfLet) of
                                                      { !_infoOisDeclOfLet ->
                                                      (case (concat $ intersperse "," semNames_) of
                                                       { !_exports ->
                                                       (case ((semFunctions_ _semFunctionsOisDeclOfLet _semFunctionsOnested _semFunctionsOoptions _semFunctionsOoutputfile )) of
                                                        { ( !_semFunctionsIpps) ->
                                                        (case ((info_ _infoOisDeclOfLet _infoOnested _infoOoptions _infoOoutputfile )) of
                                                         { ( !_infoIpps) ->
                                                         (case (writeModule _outputfile
                                                                  [ pp $ _lhsIpragmaBlocks
                                                                  , pp $ Map.findWithDefault empty (BlockPragma, Just $ identifier name_) _lhsItextBlockMap
                                                                  , pp $ _lhsIoptionsLine
                                                                  , pp $ _lhsImoduleHeader _lhsImainName ("_" ++ name_) _exports     True
                                                                  , pp $ ("import " ++ _lhsImainName ++ "_common\n")
                                                                  , pp $ Map.findWithDefault empty (BlockImport, Just $ identifier name_) _lhsItextBlockMap
                                                                  , _commentIpp
                                                                  , vlist_sep "" _infoIpps
                                                                  , vlist_sep "" _semFunctionsIpps
                                                                  , Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap
                                                                  ]) of
                                                          { !_lhsOgenSems ->
                                                          (case (["import " ++ _lhsImainName ++ "_" ++ name_ ++ "\n"]) of
                                                           { !_lhsOimports ->
                                                           (case (_commentIpp
                                                                  :  _infoIpps
                                                                  ++ _dataDefIpps
                                                                  ++ _cataFunIpps
                                                                  ++ _semDomIpps
                                                                  ++ _semWrapperIpps
                                                                  ++ _semFunctionsIpps
                                                                  ++ [Map.findWithDefault empty (BlockOther, Just $ identifier name_) _lhsItextBlockMap]) of
                                                            { !_lhsOpps ->
                                                            ( _lhsOappendCommon,_lhsOappendMain,_lhsOgenSems,_lhsOimports,_lhsOpps) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Chunks ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         isDeclOfLet          : Bool
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         nested               : Bool
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         textBlockMap         : Map BlockInfo PP_Doc
         textBlocks           : PP_Doc
      synthesized attributes:
         appendCommon         : [[PP_Doc]]
         appendMain           : [[PP_Doc]]
         genSems              : IO ()
         imports              : [String]
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Chunk 
         child tl             : Chunks 
      alternative Nil:
-}
-- cata
sem_Chunks :: Chunks  ->
              T_Chunks 
sem_Chunks !list  =
    (Prelude.foldr sem_Chunks_Cons sem_Chunks_Nil (Prelude.map sem_Chunk list) )
-- semantic domain
newtype T_Chunks  = T_Chunks (PP_Doc ->
                              Bool ->
                              String ->
                              String ->
                              (String -> String -> String -> Bool -> String) ->
                              Bool ->
                              Options ->
                              String ->
                              String ->
                              (Map BlockInfo PP_Doc) ->
                              PP_Doc ->
                              ( ([[PP_Doc]]),([[PP_Doc]]),(IO ()),([String]),PP_Docs))
data Inh_Chunks  = Inh_Chunks {importBlocks_Inh_Chunks :: !(PP_Doc),isDeclOfLet_Inh_Chunks :: !(Bool),mainFile_Inh_Chunks :: !(String),mainName_Inh_Chunks :: !(String),moduleHeader_Inh_Chunks :: !(String -> String -> String -> Bool -> String),nested_Inh_Chunks :: !(Bool),options_Inh_Chunks :: !(Options),optionsLine_Inh_Chunks :: !(String),pragmaBlocks_Inh_Chunks :: !(String),textBlockMap_Inh_Chunks :: !(Map BlockInfo PP_Doc),textBlocks_Inh_Chunks :: !(PP_Doc)}
data Syn_Chunks  = Syn_Chunks {appendCommon_Syn_Chunks :: !([[PP_Doc]]),appendMain_Syn_Chunks :: !([[PP_Doc]]),genSems_Syn_Chunks :: !(IO ()),imports_Syn_Chunks :: !([String]),pps_Syn_Chunks :: !(PP_Docs)}
wrap_Chunks :: T_Chunks  ->
               Inh_Chunks  ->
               Syn_Chunks 
wrap_Chunks !(T_Chunks sem ) !(Inh_Chunks _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks )  =
    (let ( !_lhsOappendCommon,!_lhsOappendMain,!_lhsOgenSems,!_lhsOimports,!_lhsOpps) =
             (sem _lhsIimportBlocks _lhsIisDeclOfLet _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsInested _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks )
     in  (Syn_Chunks _lhsOappendCommon _lhsOappendMain _lhsOgenSems _lhsOimports _lhsOpps ))
sem_Chunks_Cons :: T_Chunk  ->
                   T_Chunks  ->
                   T_Chunks 
sem_Chunks_Cons !(T_Chunk hd_ ) !(T_Chunks tl_ )  =
    (T_Chunks (\ (!_lhsIimportBlocks)
                 (!_lhsIisDeclOfLet)
                 (!_lhsImainFile)
                 (!_lhsImainName)
                 (!_lhsImoduleHeader)
                 (!_lhsInested)
                 (!_lhsIoptions)
                 (!_lhsIoptionsLine)
                 (!_lhsIpragmaBlocks)
                 (!_lhsItextBlockMap)
                 (!_lhsItextBlocks) ->
                   (case (_lhsIoptions) of
                    { !_tlOoptions ->
                    (case (_lhsInested) of
                     { !_tlOnested ->
                     (case (_lhsImainFile) of
                      { !_tlOmainFile ->
                      (case (_lhsIisDeclOfLet) of
                       { !_tlOisDeclOfLet ->
                       (case (_lhsIoptions) of
                        { !_hdOoptions ->
                        (case (_lhsInested) of
                         { !_hdOnested ->
                         (case (_lhsImainFile) of
                          { !_hdOmainFile ->
                          (case (_lhsIisDeclOfLet) of
                           { !_hdOisDeclOfLet ->
                           (case (_lhsItextBlocks) of
                            { !_tlOtextBlocks ->
                            (case (_lhsItextBlockMap) of
                             { !_tlOtextBlockMap ->
                             (case (_lhsIpragmaBlocks) of
                              { !_tlOpragmaBlocks ->
                              (case (_lhsIoptionsLine) of
                               { !_tlOoptionsLine ->
                               (case (_lhsImoduleHeader) of
                                { !_tlOmoduleHeader ->
                                (case (_lhsImainName) of
                                 { !_tlOmainName ->
                                 (case (_lhsIimportBlocks) of
                                  { !_tlOimportBlocks ->
                                  (case ((tl_ _tlOimportBlocks _tlOisDeclOfLet _tlOmainFile _tlOmainName _tlOmoduleHeader _tlOnested _tlOoptions _tlOoptionsLine _tlOpragmaBlocks _tlOtextBlockMap _tlOtextBlocks )) of
                                   { ( !_tlIappendCommon,!_tlIappendMain,!_tlIgenSems,!_tlIimports,!_tlIpps) ->
                                   (case (_lhsItextBlocks) of
                                    { !_hdOtextBlocks ->
                                    (case (_lhsItextBlockMap) of
                                     { !_hdOtextBlockMap ->
                                     (case (_lhsIpragmaBlocks) of
                                      { !_hdOpragmaBlocks ->
                                      (case (_lhsIoptionsLine) of
                                       { !_hdOoptionsLine ->
                                       (case (_lhsImoduleHeader) of
                                        { !_hdOmoduleHeader ->
                                        (case (_lhsImainName) of
                                         { !_hdOmainName ->
                                         (case (_lhsIimportBlocks) of
                                          { !_hdOimportBlocks ->
                                          (case ((hd_ _hdOimportBlocks _hdOisDeclOfLet _hdOmainFile _hdOmainName _hdOmoduleHeader _hdOnested _hdOoptions _hdOoptionsLine _hdOpragmaBlocks _hdOtextBlockMap _hdOtextBlocks )) of
                                           { ( !_hdIappendCommon,!_hdIappendMain,!_hdIgenSems,!_hdIimports,!_hdIpps) ->
                                           (case (_hdIappendCommon ++ _tlIappendCommon) of
                                            { !_lhsOappendCommon ->
                                            (case (_hdIappendMain ++ _tlIappendMain) of
                                             { !_lhsOappendMain ->
                                             (case (_hdIgenSems >> _tlIgenSems) of
                                              { !_lhsOgenSems ->
                                              (case (_hdIimports ++ _tlIimports) of
                                               { !_lhsOimports ->
                                               (case (_hdIpps ++ _tlIpps) of
                                                { !_lhsOpps ->
                                                ( _lhsOappendCommon,_lhsOappendMain,_lhsOgenSems,_lhsOimports,_lhsOpps) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Chunks_Nil :: T_Chunks 
sem_Chunks_Nil  =
    (T_Chunks (\ (!_lhsIimportBlocks)
                 (!_lhsIisDeclOfLet)
                 (!_lhsImainFile)
                 (!_lhsImainName)
                 (!_lhsImoduleHeader)
                 (!_lhsInested)
                 (!_lhsIoptions)
                 (!_lhsIoptionsLine)
                 (!_lhsIpragmaBlocks)
                 (!_lhsItextBlockMap)
                 (!_lhsItextBlocks) ->
                   (case ([]) of
                    { !_lhsOappendCommon ->
                    (case ([]) of
                     { !_lhsOappendMain ->
                     (case (return ()) of
                      { !_lhsOgenSems ->
                      (case ([]) of
                       { !_lhsOimports ->
                       (case ([]) of
                        { !_lhsOpps ->
                        ( _lhsOappendCommon,_lhsOappendMain,_lhsOgenSems,_lhsOimports,_lhsOpps) }) }) }) }) })) )
-- DataAlt -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         strictPre            : PP_Doc
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative DataAlt:
         child name           : {String}
         child args           : {[String]}
      alternative Record:
         child name           : {String}
         child args           : {[(String,String)]}
-}
-- cata
sem_DataAlt :: DataAlt  ->
               T_DataAlt 
sem_DataAlt !(DataAlt _name _args )  =
    (sem_DataAlt_DataAlt _name _args )
sem_DataAlt !(Record _name _args )  =
    (sem_DataAlt_Record _name _args )
-- semantic domain
newtype T_DataAlt  = T_DataAlt (Bool ->
                                PP_Doc ->
                                ( PP_Doc))
data Inh_DataAlt  = Inh_DataAlt {nested_Inh_DataAlt :: !(Bool),strictPre_Inh_DataAlt :: !(PP_Doc)}
data Syn_DataAlt  = Syn_DataAlt {pp_Syn_DataAlt :: !(PP_Doc)}
wrap_DataAlt :: T_DataAlt  ->
                Inh_DataAlt  ->
                Syn_DataAlt 
wrap_DataAlt !(T_DataAlt sem ) !(Inh_DataAlt _lhsInested _lhsIstrictPre )  =
    (let ( !_lhsOpp) =
             (sem _lhsInested _lhsIstrictPre )
     in  (Syn_DataAlt _lhsOpp ))
sem_DataAlt_DataAlt :: String ->
                       ([String]) ->
                       T_DataAlt 
sem_DataAlt_DataAlt !name_ !args_  =
    (T_DataAlt (\ (!_lhsInested)
                  (!_lhsIstrictPre) ->
                    (case (name_ >#< hv_sp (map ((_lhsIstrictPre >|<) . pp_parens . text) args_)) of
                     { !_lhsOpp ->
                     ( _lhsOpp) })) )
sem_DataAlt_Record :: String ->
                      ([(String,String)]) ->
                      T_DataAlt 
sem_DataAlt_Record !name_ !args_  =
    (T_DataAlt (\ (!_lhsInested)
                  (!_lhsIstrictPre) ->
                    (case (name_ >#< pp_block "{" "}" ","
                                              [ f >#< "::" >#< t | (f,t) <- args_ ]) of
                     { !_lhsOpp ->
                     ( _lhsOpp) })) )
-- DataAlts ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         strictPre            : PP_Doc
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : DataAlt 
         child tl             : DataAlts 
      alternative Nil:
-}
-- cata
sem_DataAlts :: DataAlts  ->
                T_DataAlts 
sem_DataAlts !list  =
    (Prelude.foldr sem_DataAlts_Cons sem_DataAlts_Nil (Prelude.map sem_DataAlt list) )
-- semantic domain
newtype T_DataAlts  = T_DataAlts (Bool ->
                                  PP_Doc ->
                                  ( PP_Docs))
data Inh_DataAlts  = Inh_DataAlts {nested_Inh_DataAlts :: !(Bool),strictPre_Inh_DataAlts :: !(PP_Doc)}
data Syn_DataAlts  = Syn_DataAlts {pps_Syn_DataAlts :: !(PP_Docs)}
wrap_DataAlts :: T_DataAlts  ->
                 Inh_DataAlts  ->
                 Syn_DataAlts 
wrap_DataAlts !(T_DataAlts sem ) !(Inh_DataAlts _lhsInested _lhsIstrictPre )  =
    (let ( !_lhsOpps) =
             (sem _lhsInested _lhsIstrictPre )
     in  (Syn_DataAlts _lhsOpps ))
sem_DataAlts_Cons :: T_DataAlt  ->
                     T_DataAlts  ->
                     T_DataAlts 
sem_DataAlts_Cons !(T_DataAlt hd_ ) !(T_DataAlts tl_ )  =
    (T_DataAlts (\ (!_lhsInested)
                   (!_lhsIstrictPre) ->
                     (case (_lhsIstrictPre) of
                      { !_tlOstrictPre ->
                      (case (_lhsIstrictPre) of
                       { !_hdOstrictPre ->
                       (case (_lhsInested) of
                        { !_tlOnested ->
                        (case ((tl_ _tlOnested _tlOstrictPre )) of
                         { ( !_tlIpps) ->
                         (case (_lhsInested) of
                          { !_hdOnested ->
                          (case ((hd_ _hdOnested _hdOstrictPre )) of
                           { ( !_hdIpp) ->
                           (case (_hdIpp : _tlIpps) of
                            { !_lhsOpps ->
                            ( _lhsOpps) }) }) }) }) }) }) })) )
sem_DataAlts_Nil :: T_DataAlts 
sem_DataAlts_Nil  =
    (T_DataAlts (\ (!_lhsInested)
                   (!_lhsIstrictPre) ->
                     (case ([]) of
                      { !_lhsOpps ->
                      ( _lhsOpps) })) )
-- Decl --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDeclOfLet          : Bool
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Comment:
         child txt            : {String}
      alternative Data:
         child name           : {String}
         child params         : {[String]}
         child alts           : DataAlts 
         child strict         : {Bool}
         child derivings      : {[String]}
      alternative Decl:
         child left           : Lhs 
         child rhs            : Expr 
         child binds          : {Set String}
         child uses           : {Set String}
      alternative NewType:
         child name           : {String}
         child params         : {[String]}
         child con            : {String}
         child tp             : Type 
      alternative PragmaDecl:
         child txt            : {String}
      alternative TSig:
         child name           : {String}
         child tp             : Type 
      alternative Type:
         child name           : {String}
         child params         : {[String]}
         child tp             : Type 
-}
-- cata
sem_Decl :: Decl  ->
            T_Decl 
sem_Decl !(Comment _txt )  =
    (sem_Decl_Comment _txt )
sem_Decl !(Data _name _params _alts _strict _derivings )  =
    (sem_Decl_Data _name _params (sem_DataAlts _alts ) _strict _derivings )
sem_Decl !(Decl _left _rhs _binds _uses )  =
    (sem_Decl_Decl (sem_Lhs _left ) (sem_Expr _rhs ) _binds _uses )
sem_Decl !(NewType _name _params _con _tp )  =
    (sem_Decl_NewType _name _params _con (sem_Type _tp ) )
sem_Decl !(PragmaDecl _txt )  =
    (sem_Decl_PragmaDecl _txt )
sem_Decl !(TSig _name _tp )  =
    (sem_Decl_TSig _name (sem_Type _tp ) )
sem_Decl !(Type _name _params _tp )  =
    (sem_Decl_Type _name _params (sem_Type _tp ) )
-- semantic domain
newtype T_Decl  = T_Decl (Bool ->
                          Bool ->
                          Options ->
                          String ->
                          ( PP_Doc))
data Inh_Decl  = Inh_Decl {isDeclOfLet_Inh_Decl :: !(Bool),nested_Inh_Decl :: !(Bool),options_Inh_Decl :: !(Options),outputfile_Inh_Decl :: !(String)}
data Syn_Decl  = Syn_Decl {pp_Syn_Decl :: !(PP_Doc)}
wrap_Decl :: T_Decl  ->
             Inh_Decl  ->
             Syn_Decl 
wrap_Decl !(T_Decl sem ) !(Inh_Decl _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile )  =
    (let ( !_lhsOpp) =
             (sem _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile )
     in  (Syn_Decl _lhsOpp ))
sem_Decl_Comment :: String ->
                    T_Decl 
sem_Decl_Comment !txt_  =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (if '\n' `elem` txt_
                          then "{-" >-< vlist (lines txt_) >-< "-}"
                          else "--" >#< txt_) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })) )
sem_Decl_Data :: String ->
                 ([String]) ->
                 T_DataAlts  ->
                 Bool ->
                 ([String]) ->
                 T_Decl 
sem_Decl_Data !name_ !params_ !(T_DataAlts alts_ ) !strict_ !derivings_  =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (if strict_ then pp "!" else empty) of
                  { !_altsOstrictPre ->
                  (case (_lhsInested) of
                   { !_altsOnested ->
                   (case ((alts_ _altsOnested _altsOstrictPre )) of
                    { ( !_altsIpps) ->
                    (case ("data" >#< hv_sp (name_ : params_)
                           >#<  ( case _altsIpps of
                                        [] -> empty
                                        (x:xs) ->              "=" >#<  x
                                               >-< vlist (map ("|" >#<) xs)
                                   >-< if null derivings_
                                          then empty
                                          else "deriving" >#< ppTuple False (map text derivings_)
                                )) of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }) }) })) )
sem_Decl_Decl :: T_Lhs  ->
                 T_Expr  ->
                 (Set String) ->
                 (Set String) ->
                 T_Decl 
sem_Decl_Decl !(T_Lhs left_ ) !(T_Expr rhs_ ) !binds_ !uses_  =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_rhsOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_rhsOoptions ->
                   (case (_lhsInested) of
                    { !_rhsOnested ->
                    (case (_lhsIoutputfile) of
                     { !_leftOoutputfile ->
                     (case (_lhsIoptions) of
                      { !_leftOoptions ->
                      (case (_lhsInested) of
                       { !_leftOnested ->
                       (case (_lhsIisDeclOfLet) of
                        { !_leftOisDeclOfLet ->
                        (case ((rhs_ _rhsOnested _rhsOoptions _rhsOoutputfile )) of
                         { ( !_rhsIpp) ->
                         (case ((left_ _leftOisDeclOfLet _leftOnested _leftOoptions _leftOoutputfile )) of
                          { ( !_leftIpp) ->
                          (case (_leftIpp >#< "="
                                 >-< indent 4 _rhsIpp) of
                           { !_lhsOpp ->
                           ( _lhsOpp) }) }) }) }) }) }) }) }) }) })) )
sem_Decl_NewType :: String ->
                    ([String]) ->
                    String ->
                    T_Type  ->
                    T_Decl 
sem_Decl_NewType !name_ !params_ !con_ !(T_Type tp_ )  =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsInested) of
                  { !_tpOnested ->
                  (case ((tp_ _tpOnested )) of
                   { ( !_tpIpp,!_tpIprec) ->
                   (case ("newtype" >#< hv_sp (name_ : params_) >#< "=" >#< con_ >#< pp_parens _tpIpp) of
                    { !_lhsOpp ->
                    ( _lhsOpp) }) }) })) )
sem_Decl_PragmaDecl :: String ->
                       T_Decl 
sem_Decl_PragmaDecl !txt_  =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case ("{-#" >#< text txt_ >#< "#-}") of
                  { !_lhsOpp ->
                  ( _lhsOpp) })) )
sem_Decl_TSig :: String ->
                 T_Type  ->
                 T_Decl 
sem_Decl_TSig !name_ !(T_Type tp_ )  =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsInested) of
                  { !_tpOnested ->
                  (case ((tp_ _tpOnested )) of
                   { ( !_tpIpp,!_tpIprec) ->
                   (case (name_ >#< "::" >#< _tpIpp) of
                    { !_lhsOpp ->
                    ( _lhsOpp) }) }) })) )
sem_Decl_Type :: String ->
                 ([String]) ->
                 T_Type  ->
                 T_Decl 
sem_Decl_Type !name_ !params_ !(T_Type tp_ )  =
    (T_Decl (\ (!_lhsIisDeclOfLet)
               (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsInested) of
                  { !_tpOnested ->
                  (case ((tp_ _tpOnested )) of
                   { ( !_tpIpp,!_tpIprec) ->
                   (case ("type" >#< hv_sp (name_ : params_) >#< "=" >#<  _tpIpp) of
                    { !_lhsOpp ->
                    ( _lhsOpp) }) }) })) )
-- Decls -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDeclOfLet          : Bool
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Decl 
         child tl             : Decls 
      alternative Nil:
-}
-- cata
sem_Decls :: Decls  ->
             T_Decls 
sem_Decls !list  =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list) )
-- semantic domain
newtype T_Decls  = T_Decls (Bool ->
                            Bool ->
                            Options ->
                            String ->
                            ( PP_Docs))
data Inh_Decls  = Inh_Decls {isDeclOfLet_Inh_Decls :: !(Bool),nested_Inh_Decls :: !(Bool),options_Inh_Decls :: !(Options),outputfile_Inh_Decls :: !(String)}
data Syn_Decls  = Syn_Decls {pps_Syn_Decls :: !(PP_Docs)}
wrap_Decls :: T_Decls  ->
              Inh_Decls  ->
              Syn_Decls 
wrap_Decls !(T_Decls sem ) !(Inh_Decls _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile )  =
    (let ( !_lhsOpps) =
             (sem _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile )
     in  (Syn_Decls _lhsOpps ))
sem_Decls_Cons :: T_Decl  ->
                  T_Decls  ->
                  T_Decls 
sem_Decls_Cons !(T_Decl hd_ ) !(T_Decls tl_ )  =
    (T_Decls (\ (!_lhsIisDeclOfLet)
                (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case (_lhsIoutputfile) of
                   { !_tlOoutputfile ->
                   (case (_lhsIoptions) of
                    { !_tlOoptions ->
                    (case (_lhsInested) of
                     { !_tlOnested ->
                     (case (_lhsIisDeclOfLet) of
                      { !_tlOisDeclOfLet ->
                      (case (_lhsIoutputfile) of
                       { !_hdOoutputfile ->
                       (case (_lhsIoptions) of
                        { !_hdOoptions ->
                        (case (_lhsInested) of
                         { !_hdOnested ->
                         (case (_lhsIisDeclOfLet) of
                          { !_hdOisDeclOfLet ->
                          (case ((tl_ _tlOisDeclOfLet _tlOnested _tlOoptions _tlOoutputfile )) of
                           { ( !_tlIpps) ->
                           (case ((hd_ _hdOisDeclOfLet _hdOnested _hdOoptions _hdOoutputfile )) of
                            { ( !_hdIpp) ->
                            (case (_hdIpp : _tlIpps) of
                             { !_lhsOpps ->
                             ( _lhsOpps) }) }) }) }) }) }) }) }) }) }) })) )
sem_Decls_Nil :: T_Decls 
sem_Decls_Nil  =
    (T_Decls (\ (!_lhsIisDeclOfLet)
                (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case ([]) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })) )
-- Expr --------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative App:
         child name           : {String}
         child args           : Exprs 
      alternative Case:
         child expr           : Expr 
         child alts           : CaseAlts 
      alternative Lambda:
         child args           : Exprs 
         child body           : Expr 
         visit 0:
            local addBang     : _
            local strictParams : _
      alternative Let:
         child decls          : Decls 
         child body           : Expr 
      alternative LineExpr:
         child expr           : Expr 
      alternative PragmaExpr:
         child onLeftSide     : {Bool}
         child onNewLine      : {Bool}
         child txt            : {String}
         child expr           : Expr 
      alternative SimpleExpr:
         child txt            : {String}
      alternative TextExpr:
         child lns            : {[String]}
      alternative Trace:
         child txt            : {String}
         child expr           : Expr 
      alternative TupleExpr:
         child exprs          : Exprs 
      alternative TypedExpr:
         child expr           : Expr 
         child tp             : Type 
      alternative UnboxedTupleExpr:
         child exprs          : Exprs 
-}
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr !(App _name _args )  =
    (sem_Expr_App _name (sem_Exprs _args ) )
sem_Expr !(Case _expr _alts )  =
    (sem_Expr_Case (sem_Expr _expr ) (sem_CaseAlts _alts ) )
sem_Expr !(Lambda _args _body )  =
    (sem_Expr_Lambda (sem_Exprs _args ) (sem_Expr _body ) )
sem_Expr !(Let _decls _body )  =
    (sem_Expr_Let (sem_Decls _decls ) (sem_Expr _body ) )
sem_Expr !(LineExpr _expr )  =
    (sem_Expr_LineExpr (sem_Expr _expr ) )
sem_Expr !(PragmaExpr _onLeftSide _onNewLine _txt _expr )  =
    (sem_Expr_PragmaExpr _onLeftSide _onNewLine _txt (sem_Expr _expr ) )
sem_Expr !(SimpleExpr _txt )  =
    (sem_Expr_SimpleExpr _txt )
sem_Expr !(TextExpr _lns )  =
    (sem_Expr_TextExpr _lns )
sem_Expr !(Trace _txt _expr )  =
    (sem_Expr_Trace _txt (sem_Expr _expr ) )
sem_Expr !(TupleExpr _exprs )  =
    (sem_Expr_TupleExpr (sem_Exprs _exprs ) )
sem_Expr !(TypedExpr _expr _tp )  =
    (sem_Expr_TypedExpr (sem_Expr _expr ) (sem_Type _tp ) )
sem_Expr !(UnboxedTupleExpr _exprs )  =
    (sem_Expr_UnboxedTupleExpr (sem_Exprs _exprs ) )
-- semantic domain
newtype T_Expr  = T_Expr (Bool ->
                          Options ->
                          String ->
                          ( PP_Doc))
data Inh_Expr  = Inh_Expr {nested_Inh_Expr :: !(Bool),options_Inh_Expr :: !(Options),outputfile_Inh_Expr :: !(String)}
data Syn_Expr  = Syn_Expr {pp_Syn_Expr :: !(PP_Doc)}
wrap_Expr :: T_Expr  ->
             Inh_Expr  ->
             Syn_Expr 
wrap_Expr !(T_Expr sem ) !(Inh_Expr _lhsInested _lhsIoptions _lhsIoutputfile )  =
    (let ( !_lhsOpp) =
             (sem _lhsInested _lhsIoptions _lhsIoutputfile )
     in  (Syn_Expr _lhsOpp ))
sem_Expr_App :: String ->
                T_Exprs  ->
                T_Expr 
sem_Expr_App !name_ !(T_Exprs args_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_argsOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_argsOoptions ->
                   (case (_lhsInested) of
                    { !_argsOnested ->
                    (case ((args_ _argsOnested _argsOoptions _argsOoutputfile )) of
                     { ( !_argsIpps) ->
                     (case (pp_parens $ name_ >#< hv_sp _argsIpps) of
                      { !_lhsOpp ->
                      ( _lhsOpp) }) }) }) }) })) )
sem_Expr_Case :: T_Expr  ->
                 T_CaseAlts  ->
                 T_Expr 
sem_Expr_Case !(T_Expr expr_ ) !(T_CaseAlts alts_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_altsOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_altsOoptions ->
                   (case (_lhsInested) of
                    { !_altsOnested ->
                    (case (_lhsIoutputfile) of
                     { !_exprOoutputfile ->
                     (case (_lhsIoptions) of
                      { !_exprOoptions ->
                      (case (_lhsInested) of
                       { !_exprOnested ->
                       (case ((alts_ _altsOnested _altsOoptions _altsOoutputfile )) of
                        { ( !_altsIpps) ->
                        (case ((expr_ _exprOnested _exprOoptions _exprOoutputfile )) of
                         { ( !_exprIpp) ->
                         (case (pp_parens (    "case" >#< pp_parens _exprIpp >#< "of"
                                          >-< (vlist _altsIpps)
                                          )) of
                          { !_lhsOpp ->
                          ( _lhsOpp) }) }) }) }) }) }) }) }) })) )
sem_Expr_Lambda :: T_Exprs  ->
                   T_Expr  ->
                   T_Expr 
sem_Expr_Lambda !(T_Exprs args_ ) !(T_Expr body_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_bodyOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_bodyOoptions ->
                   (case (_lhsInested) of
                    { !_bodyOnested ->
                    (case (_lhsIoutputfile) of
                     { !_argsOoutputfile ->
                     (case (_lhsIoptions) of
                      { !_argsOoptions ->
                      (case (_lhsInested) of
                       { !_argsOnested ->
                       (case (if bangpats _lhsIoptions
                              then \p -> pp_parens ("!" >|< p)
                              else id) of
                        { !_addBang ->
                        (case ((args_ _argsOnested _argsOoptions _argsOoutputfile )) of
                         { ( !_argsIpps) ->
                         (case (if strictSems _lhsIoptions
                                then _argsIpps
                                else []) of
                          { !_strictParams ->
                          (case ((body_ _bodyOnested _bodyOoptions _bodyOoutputfile )) of
                           { ( !_bodyIpp) ->
                           (case (pp_parens (    "\\" >#< (vlist (map _addBang     _argsIpps)) >#< "->"
                                            >-< indent 4 (_strictParams     `ppMultiSeqV` _bodyIpp)
                                            )) of
                            { !_lhsOpp ->
                            ( _lhsOpp) }) }) }) }) }) }) }) }) }) }) })) )
sem_Expr_Let :: T_Decls  ->
                T_Expr  ->
                T_Expr 
sem_Expr_Let !(T_Decls decls_ ) !(T_Expr body_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_bodyOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_bodyOoptions ->
                   (case (_lhsInested) of
                    { !_bodyOnested ->
                    (case (_lhsIoutputfile) of
                     { !_declsOoutputfile ->
                     (case (_lhsIoptions) of
                      { !_declsOoptions ->
                      (case (_lhsInested) of
                       { !_declsOnested ->
                       (case (True) of
                        { !_declsOisDeclOfLet ->
                        (case ((body_ _bodyOnested _bodyOoptions _bodyOoutputfile )) of
                         { ( !_bodyIpp) ->
                         (case ((decls_ _declsOisDeclOfLet _declsOnested _declsOoptions _declsOoutputfile )) of
                          { ( !_declsIpps) ->
                          (case (pp_parens (    "let" >#< (vlist _declsIpps)
                                           >-< "in " >#< _bodyIpp
                                           )) of
                           { !_lhsOpp ->
                           ( _lhsOpp) }) }) }) }) }) }) }) }) }) })) )
sem_Expr_LineExpr :: T_Expr  ->
                     T_Expr 
sem_Expr_LineExpr !(T_Expr expr_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_exprOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_exprOoptions ->
                   (case (_lhsInested) of
                    { !_exprOnested ->
                    (case ((expr_ _exprOnested _exprOoptions _exprOoutputfile )) of
                     { ( !_exprIpp) ->
                     (case (_exprIpp >-< "{-# LINE" >#< ppWithLineNr (\n -> pp $ show $ n + 1) >#< show _lhsIoutputfile >#< "#-}") of
                      { !_lhsOpp ->
                      ( _lhsOpp) }) }) }) }) })) )
sem_Expr_PragmaExpr :: Bool ->
                       Bool ->
                       String ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_PragmaExpr !onLeftSide_ !onNewLine_ !txt_ !(T_Expr expr_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_exprOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_exprOoptions ->
                   (case (_lhsInested) of
                    { !_exprOnested ->
                    (case ((expr_ _exprOnested _exprOoptions _exprOoutputfile )) of
                     { ( !_exprIpp) ->
                     (case (let pragmaDoc = "{-#" >#< txt_ >#< "#-}"
                                op = if onNewLine_
                                     then (>-<)
                                     else (>#<)
                                leftOp x y = if onLeftSide_
                                             then x `op` y
                                             else y
                                rightOp x y = if onLeftSide_
                                              then x
                                              else x `op` y
                            in pragmaDoc `leftOp` _exprIpp `rightOp` pragmaDoc) of
                      { !_lhsOpp ->
                      ( _lhsOpp) }) }) }) }) })) )
sem_Expr_SimpleExpr :: String ->
                       T_Expr 
sem_Expr_SimpleExpr !txt_  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (text txt_) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })) )
sem_Expr_TextExpr :: ([String]) ->
                     T_Expr 
sem_Expr_TextExpr !lns_  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (vlist (map text lns_)) of
                  { !_lhsOpp ->
                  ( _lhsOpp) })) )
sem_Expr_Trace :: String ->
                  T_Expr  ->
                  T_Expr 
sem_Expr_Trace !txt_ !(T_Expr expr_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_exprOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_exprOoptions ->
                   (case (_lhsInested) of
                    { !_exprOnested ->
                    (case ((expr_ _exprOnested _exprOoptions _exprOoutputfile )) of
                     { ( !_exprIpp) ->
                     (case ("trace" >#< (   pp_parens ("\"" >|< text txt_ >|< "\"")
                                        >-< pp_parens _exprIpp
                                        )) of
                      { !_lhsOpp ->
                      ( _lhsOpp) }) }) }) }) })) )
sem_Expr_TupleExpr :: T_Exprs  ->
                      T_Expr 
sem_Expr_TupleExpr !(T_Exprs exprs_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_exprsOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_exprsOoptions ->
                   (case (_lhsInested) of
                    { !_exprsOnested ->
                    (case ((exprs_ _exprsOnested _exprsOoptions _exprsOoutputfile )) of
                     { ( !_exprsIpps) ->
                     (case (ppTuple _lhsInested _exprsIpps) of
                      { !_lhsOpp ->
                      ( _lhsOpp) }) }) }) }) })) )
sem_Expr_TypedExpr :: T_Expr  ->
                      T_Type  ->
                      T_Expr 
sem_Expr_TypedExpr !(T_Expr expr_ ) !(T_Type tp_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsInested) of
                  { !_tpOnested ->
                  (case (_lhsIoutputfile) of
                   { !_exprOoutputfile ->
                   (case (_lhsIoptions) of
                    { !_exprOoptions ->
                    (case (_lhsInested) of
                     { !_exprOnested ->
                     (case ((tp_ _tpOnested )) of
                      { ( !_tpIpp,!_tpIprec) ->
                      (case ((expr_ _exprOnested _exprOoptions _exprOoutputfile )) of
                       { ( !_exprIpp) ->
                       (case (pp_parens (_exprIpp >#< "::" >#< _tpIpp)) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) }) }) }) }) })) )
sem_Expr_UnboxedTupleExpr :: T_Exprs  ->
                             T_Expr 
sem_Expr_UnboxedTupleExpr !(T_Exprs exprs_ )  =
    (T_Expr (\ (!_lhsInested)
               (!_lhsIoptions)
               (!_lhsIoutputfile) ->
                 (case (_lhsIoutputfile) of
                  { !_exprsOoutputfile ->
                  (case (_lhsIoptions) of
                   { !_exprsOoptions ->
                   (case (_lhsInested) of
                    { !_exprsOnested ->
                    (case ((exprs_ _exprsOnested _exprsOoptions _exprsOoutputfile )) of
                     { ( !_exprsIpps) ->
                     (case (ppUnboxedTuple _lhsInested _exprsIpps) of
                      { !_lhsOpp ->
                      ( _lhsOpp) }) }) }) }) })) )
-- Exprs -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Expr 
         child tl             : Exprs 
      alternative Nil:
-}
-- cata
sem_Exprs :: Exprs  ->
             T_Exprs 
sem_Exprs !list  =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list) )
-- semantic domain
newtype T_Exprs  = T_Exprs (Bool ->
                            Options ->
                            String ->
                            ( PP_Docs))
data Inh_Exprs  = Inh_Exprs {nested_Inh_Exprs :: !(Bool),options_Inh_Exprs :: !(Options),outputfile_Inh_Exprs :: !(String)}
data Syn_Exprs  = Syn_Exprs {pps_Syn_Exprs :: !(PP_Docs)}
wrap_Exprs :: T_Exprs  ->
              Inh_Exprs  ->
              Syn_Exprs 
wrap_Exprs !(T_Exprs sem ) !(Inh_Exprs _lhsInested _lhsIoptions _lhsIoutputfile )  =
    (let ( !_lhsOpps) =
             (sem _lhsInested _lhsIoptions _lhsIoutputfile )
     in  (Syn_Exprs _lhsOpps ))
sem_Exprs_Cons :: T_Expr  ->
                  T_Exprs  ->
                  T_Exprs 
sem_Exprs_Cons !(T_Expr hd_ ) !(T_Exprs tl_ )  =
    (T_Exprs (\ (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case (_lhsIoutputfile) of
                   { !_tlOoutputfile ->
                   (case (_lhsIoptions) of
                    { !_tlOoptions ->
                    (case (_lhsInested) of
                     { !_tlOnested ->
                     (case (_lhsIoutputfile) of
                      { !_hdOoutputfile ->
                      (case (_lhsIoptions) of
                       { !_hdOoptions ->
                       (case (_lhsInested) of
                        { !_hdOnested ->
                        (case ((tl_ _tlOnested _tlOoptions _tlOoutputfile )) of
                         { ( !_tlIpps) ->
                         (case ((hd_ _hdOnested _hdOoptions _hdOoutputfile )) of
                          { ( !_hdIpp) ->
                          (case (_hdIpp : _tlIpps) of
                           { !_lhsOpps ->
                           ( _lhsOpps) }) }) }) }) }) }) }) }) })) )
sem_Exprs_Nil :: T_Exprs 
sem_Exprs_Nil  =
    (T_Exprs (\ (!_lhsInested)
                (!_lhsIoptions)
                (!_lhsIoutputfile) ->
                  (case ([]) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })) )
-- Lhs ---------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         isDeclOfLet          : Bool
         nested               : Bool
         options              : Options
         outputfile           : String
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Fun:
         child name           : {String}
         child args           : Exprs 
         visit 0:
            local addBang     : _
            local strictGuard : _
            local hasStrictVars : _
            local addStrictGuard : _
      alternative Pattern3:
         child pat3           : Pattern 
         visit 0:
            local hasStrictVars : _
            local strictGuard : _
            local addStrictGuard : _
      alternative Pattern3SM:
         child pat3           : Pattern 
      alternative TupleLhs:
         child comps          : {[String]}
         visit 0:
            local addBang     : _
            local hasStrictVars : _
            local strictGuard : _
            local addStrictGuard : _
      alternative UnboxedTupleLhs:
         child comps          : {[String]}
         visit 0:
            local addBang     : _
            local hasStrictVars : _
            local strictGuard : _
            local addStrictGuard : _
-}
-- cata
sem_Lhs :: Lhs  ->
           T_Lhs 
sem_Lhs !(Fun _name _args )  =
    (sem_Lhs_Fun _name (sem_Exprs _args ) )
sem_Lhs !(Pattern3 _pat3 )  =
    (sem_Lhs_Pattern3 (sem_Pattern _pat3 ) )
sem_Lhs !(Pattern3SM _pat3 )  =
    (sem_Lhs_Pattern3SM (sem_Pattern _pat3 ) )
sem_Lhs !(TupleLhs _comps )  =
    (sem_Lhs_TupleLhs _comps )
sem_Lhs !(UnboxedTupleLhs _comps )  =
    (sem_Lhs_UnboxedTupleLhs _comps )
-- semantic domain
newtype T_Lhs  = T_Lhs (Bool ->
                        Bool ->
                        Options ->
                        String ->
                        ( PP_Doc))
data Inh_Lhs  = Inh_Lhs {isDeclOfLet_Inh_Lhs :: !(Bool),nested_Inh_Lhs :: !(Bool),options_Inh_Lhs :: !(Options),outputfile_Inh_Lhs :: !(String)}
data Syn_Lhs  = Syn_Lhs {pp_Syn_Lhs :: !(PP_Doc)}
wrap_Lhs :: T_Lhs  ->
            Inh_Lhs  ->
            Syn_Lhs 
wrap_Lhs !(T_Lhs sem ) !(Inh_Lhs _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile )  =
    (let ( !_lhsOpp) =
             (sem _lhsIisDeclOfLet _lhsInested _lhsIoptions _lhsIoutputfile )
     in  (Syn_Lhs _lhsOpp ))
sem_Lhs_Fun :: String ->
               T_Exprs  ->
               T_Lhs 
sem_Lhs_Fun !name_ !(T_Exprs args_ )  =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (_lhsIoutputfile) of
                 { !_argsOoutputfile ->
                 (case (_lhsIoptions) of
                  { !_argsOoptions ->
                  (case (_lhsInested) of
                   { !_argsOnested ->
                   (case (if bangpats _lhsIoptions
                                   then \p -> "!" >|< p
                                   else id) of
                    { !_addBang ->
                    (case ((args_ _argsOnested _argsOoptions _argsOoutputfile )) of
                     { ( !_argsIpps) ->
                     (case (_argsIpps `ppMultiSeqH` (pp "True")) of
                      { !_strictGuard ->
                      (case (not (null _argsIpps)) of
                       { !_hasStrictVars ->
                       (case (if strictSems _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id) of
                        { !_addStrictGuard ->
                        (case (_addStrictGuard     (name_ >#< hv_sp (map _addBang     _argsIpps))) of
                         { !_lhsOpp ->
                         ( _lhsOpp) }) }) }) }) }) }) }) }) })) )
sem_Lhs_Pattern3 :: T_Pattern  ->
                    T_Lhs 
sem_Lhs_Pattern3 !(T_Pattern pat3_ )  =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (_lhsIoptions) of
                 { !_pat3Ooptions ->
                 (case (_lhsIisDeclOfLet) of
                  { !_pat3OisDeclOfLet ->
                  (case (False) of
                   { !_pat3ObelowIrrefutable ->
                   (case ((pat3_ _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions )) of
                    { ( !_pat3Icopy,!_pat3IisUnderscore,!_pat3Ipp,!_pat3Ipp',!_pat3IstrictVars) ->
                    (case (not (null _pat3IstrictVars)) of
                     { !_hasStrictVars ->
                     (case (_pat3IstrictVars `ppMultiSeqH` (pp "True")) of
                      { !_strictGuard ->
                      (case (if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id) of
                       { !_addStrictGuard ->
                       (case (_addStrictGuard     _pat3Ipp) of
                        { !_lhsOpp ->
                        ( _lhsOpp) }) }) }) }) }) }) }) })) )
sem_Lhs_Pattern3SM :: T_Pattern  ->
                      T_Lhs 
sem_Lhs_Pattern3SM !(T_Pattern pat3_ )  =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (_lhsIoptions) of
                 { !_pat3Ooptions ->
                 (case (_lhsIisDeclOfLet) of
                  { !_pat3OisDeclOfLet ->
                  (case (False) of
                   { !_pat3ObelowIrrefutable ->
                   (case ((pat3_ _pat3ObelowIrrefutable _pat3OisDeclOfLet _pat3Ooptions )) of
                    { ( !_pat3Icopy,!_pat3IisUnderscore,!_pat3Ipp,!_pat3Ipp',!_pat3IstrictVars) ->
                    (case (_pat3Ipp') of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }) }) }) })) )
sem_Lhs_TupleLhs :: ([String]) ->
                    T_Lhs 
sem_Lhs_TupleLhs !comps_  =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (if bangpats _lhsIoptions
                                then \p -> "!" >|< p
                                else id) of
                 { !_addBang ->
                 (case (not (null comps_)) of
                  { !_hasStrictVars ->
                  (case (if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                         then map text comps_ `ppMultiSeqH` (pp "True")
                         else pp "True") of
                   { !_strictGuard ->
                   (case (if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id) of
                    { !_addStrictGuard ->
                    (case (_addStrictGuard     $ ppTuple _lhsInested (map (_addBang     . text) comps_)) of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }) }) }) })) )
sem_Lhs_UnboxedTupleLhs :: ([String]) ->
                           T_Lhs 
sem_Lhs_UnboxedTupleLhs !comps_  =
    (T_Lhs (\ (!_lhsIisDeclOfLet)
              (!_lhsInested)
              (!_lhsIoptions)
              (!_lhsIoutputfile) ->
                (case (if bangpats _lhsIoptions
                                then \p -> "!" >|< p
                                else id) of
                 { !_addBang ->
                 (case (not (null comps_)) of
                  { !_hasStrictVars ->
                  (case (if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                         then map text comps_ `ppMultiSeqH` (pp "True")
                         else pp "True") of
                   { !_strictGuard ->
                   (case (if strictCases _lhsIoptions && _hasStrictVars     then \v -> v >#< "|" >#< _strictGuard     else id) of
                    { !_addStrictGuard ->
                    (case (_addStrictGuard     $ ppUnboxedTuple _lhsInested (map (_addBang     . text) comps_)) of
                     { !_lhsOpp ->
                     ( _lhsOpp) }) }) }) }) })) )
-- Pattern -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         belowIrrefutable     : Bool
         isDeclOfLet          : Bool
         options              : Options
      synthesized attributes:
         copy                 : SELF 
         isUnderscore         : Bool
         pp                   : PP_Doc
         pp'                  : PP_Doc
         strictVars           : [PP_Doc]
   alternatives:
      alternative Alias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : Pattern 
         child parts          : Patterns 
         visit 0:
            local copy        : _
            local ppVar       : _
            local addBang     : _
            local ppVarBang   : _
            local strictPatVars : _
            local strictVar   : _
      alternative Constr:
         child name           : {ConstructorIdent}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local addBang     : _
      alternative Irrefutable:
         child pat            : Pattern 
         visit 0:
            local copy        : _
      alternative Product:
         child pos            : {Pos}
         child pats           : Patterns 
         visit 0:
            local copy        : _
            local addBang     : _
      alternative Underscore:
         child pos            : {Pos}
         visit 0:
            local copy        : _
-}
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern !(Alias _field _attr _pat _parts )  =
    (sem_Pattern_Alias _field _attr (sem_Pattern _pat ) (sem_Patterns _parts ) )
sem_Pattern !(Constr _name _pats )  =
    (sem_Pattern_Constr _name (sem_Patterns _pats ) )
sem_Pattern !(Irrefutable _pat )  =
    (sem_Pattern_Irrefutable (sem_Pattern _pat ) )
sem_Pattern !(Product _pos _pats )  =
    (sem_Pattern_Product _pos (sem_Patterns _pats ) )
sem_Pattern !(Underscore _pos )  =
    (sem_Pattern_Underscore _pos )
-- semantic domain
newtype T_Pattern  = T_Pattern (Bool ->
                                Bool ->
                                Options ->
                                ( Pattern,Bool,PP_Doc,PP_Doc,([PP_Doc])))
data Inh_Pattern  = Inh_Pattern {belowIrrefutable_Inh_Pattern :: !(Bool),isDeclOfLet_Inh_Pattern :: !(Bool),options_Inh_Pattern :: !(Options)}
data Syn_Pattern  = Syn_Pattern {copy_Syn_Pattern :: !(Pattern),isUnderscore_Syn_Pattern :: !(Bool),pp_Syn_Pattern :: !(PP_Doc),pp'_Syn_Pattern :: !(PP_Doc),strictVars_Syn_Pattern :: !([PP_Doc])}
wrap_Pattern :: T_Pattern  ->
                Inh_Pattern  ->
                Syn_Pattern 
wrap_Pattern !(T_Pattern sem ) !(Inh_Pattern _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions )  =
    (let ( !_lhsOcopy,!_lhsOisUnderscore,!_lhsOpp,!_lhsOpp',!_lhsOstrictVars) =
             (sem _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions )
     in  (Syn_Pattern _lhsOcopy _lhsOisUnderscore _lhsOpp _lhsOpp' _lhsOstrictVars ))
sem_Pattern_Alias :: Identifier ->
                     Identifier ->
                     T_Pattern  ->
                     T_Patterns  ->
                     T_Pattern 
sem_Pattern_Alias !field_ !attr_ !(T_Pattern pat_ ) !(T_Patterns parts_ )  =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (_lhsIoptions) of
                     { !_partsOoptions ->
                     (case (_lhsIisDeclOfLet) of
                      { !_partsOisDeclOfLet ->
                      (case (_lhsIbelowIrrefutable) of
                       { !_partsObelowIrrefutable ->
                       (case ((parts_ _partsObelowIrrefutable _partsOisDeclOfLet _partsOoptions )) of
                        { ( !_partsIcopy,!_partsIpps,!_partsIpps',!_partsIstrictVars) ->
                        (case (_lhsIoptions) of
                         { !_patOoptions ->
                         (case (_lhsIisDeclOfLet) of
                          { !_patOisDeclOfLet ->
                          (case (_lhsIbelowIrrefutable) of
                           { !_patObelowIrrefutable ->
                           (case ((pat_ _patObelowIrrefutable _patOisDeclOfLet _patOoptions )) of
                            { ( !_patIcopy,!_patIisUnderscore,!_patIpp,!_patIpp',!_patIstrictVars) ->
                            (case (Alias field_ attr_ _patIcopy _partsIcopy) of
                             { !_copy ->
                             (case (_copy) of
                              { !_lhsOcopy ->
                              (case (False) of
                               { !_lhsOisUnderscore ->
                               (case (pp (attrname False field_ attr_)) of
                                { !_ppVar ->
                                (case (if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                                       then \p -> "!" >|< p
                                       else id) of
                                 { !_addBang ->
                                 (case (_addBang     $ _ppVar) of
                                  { !_ppVarBang ->
                                  (case (if _patIisUnderscore
                                          then _ppVarBang
                                          else _ppVarBang     >|< "@" >|< _patIpp) of
                                   { !_lhsOpp ->
                                   (case (let attribute | field_ == _LOC || field_ == nullIdent = locname' attr_
                                                        | otherwise                             = attrname False field_ attr_
                                          in attribute >|< "@" >|< _patIpp') of
                                    { !_lhsOpp' ->
                                    (case (if stricterCases _lhsIoptions && not _lhsIisDeclOfLet
                                           then _patIstrictVars
                                           else []) of
                                     { !_strictPatVars ->
                                     (case (if strictCases _lhsIoptions && not _lhsIisDeclOfLet
                                            then [_ppVar    ]
                                            else []) of
                                      { !_strictVar ->
                                      (case (_strictVar     ++ _strictPatVars) of
                                       { !_lhsOstrictVars ->
                                       ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Constr :: ConstructorIdent ->
                      T_Patterns  ->
                      T_Pattern 
sem_Pattern_Constr !name_ !(T_Patterns pats_ )  =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (_lhsIoptions) of
                     { !_patsOoptions ->
                     (case (_lhsIisDeclOfLet) of
                      { !_patsOisDeclOfLet ->
                      (case (_lhsIbelowIrrefutable) of
                       { !_patsObelowIrrefutable ->
                       (case ((pats_ _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions )) of
                        { ( !_patsIcopy,!_patsIpps,!_patsIpps',!_patsIstrictVars) ->
                        (case (Constr name_ _patsIcopy) of
                         { !_copy ->
                         (case (_copy) of
                          { !_lhsOcopy ->
                          (case (False) of
                           { !_lhsOisUnderscore ->
                           (case (if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                                  then \p -> "!" >|< p
                                  else id) of
                            { !_addBang ->
                            (case (_addBang     $ pp_parens $ name_ >#< hv_sp _patsIpps) of
                             { !_lhsOpp ->
                             (case (pp_parens $ name_ >#< hv_sp (map pp_parens _patsIpps')) of
                              { !_lhsOpp' ->
                              (case (_patsIstrictVars) of
                               { !_lhsOstrictVars ->
                               ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Irrefutable :: T_Pattern  ->
                           T_Pattern 
sem_Pattern_Irrefutable !(T_Pattern pat_ )  =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (_lhsIoptions) of
                     { !_patOoptions ->
                     (case (_lhsIisDeclOfLet) of
                      { !_patOisDeclOfLet ->
                      (case (True) of
                       { !_patObelowIrrefutable ->
                       (case ((pat_ _patObelowIrrefutable _patOisDeclOfLet _patOoptions )) of
                        { ( !_patIcopy,!_patIisUnderscore,!_patIpp,!_patIpp',!_patIstrictVars) ->
                        (case (Irrefutable _patIcopy) of
                         { !_copy ->
                         (case (_copy) of
                          { !_lhsOcopy ->
                          (case (_patIisUnderscore) of
                           { !_lhsOisUnderscore ->
                           (case (text "~" >|< pp_parens _patIpp) of
                            { !_lhsOpp ->
                            (case (text "~" >|< pp_parens _patIpp) of
                             { !_lhsOpp' ->
                             (case ([]) of
                              { !_lhsOstrictVars ->
                              ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Product :: Pos ->
                       T_Patterns  ->
                       T_Pattern 
sem_Pattern_Product !pos_ !(T_Patterns pats_ )  =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (_lhsIoptions) of
                     { !_patsOoptions ->
                     (case (_lhsIisDeclOfLet) of
                      { !_patsOisDeclOfLet ->
                      (case (_lhsIbelowIrrefutable) of
                       { !_patsObelowIrrefutable ->
                       (case ((pats_ _patsObelowIrrefutable _patsOisDeclOfLet _patsOoptions )) of
                        { ( !_patsIcopy,!_patsIpps,!_patsIpps',!_patsIstrictVars) ->
                        (case (Product pos_ _patsIcopy) of
                         { !_copy ->
                         (case (_copy) of
                          { !_lhsOcopy ->
                          (case (False) of
                           { !_lhsOisUnderscore ->
                           (case (if bangpats _lhsIoptions && not _lhsIisDeclOfLet && not _lhsIbelowIrrefutable
                                  then \p -> "!" >|< p
                                  else id) of
                            { !_addBang ->
                            (case (_addBang     $ pp_block "(" ")" "," _patsIpps) of
                             { !_lhsOpp ->
                             (case (pp_block "(" ")" "," _patsIpps') of
                              { !_lhsOpp' ->
                              (case (_patsIstrictVars) of
                               { !_lhsOstrictVars ->
                               ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) })) )
sem_Pattern_Underscore :: Pos ->
                          T_Pattern 
sem_Pattern_Underscore !pos_  =
    (T_Pattern (\ (!_lhsIbelowIrrefutable)
                  (!_lhsIisDeclOfLet)
                  (!_lhsIoptions) ->
                    (case (Underscore pos_) of
                     { !_copy ->
                     (case (_copy) of
                      { !_lhsOcopy ->
                      (case (True) of
                       { !_lhsOisUnderscore ->
                       (case (text "_") of
                        { !_lhsOpp ->
                        (case (text "_") of
                         { !_lhsOpp' ->
                         (case ([]) of
                          { !_lhsOstrictVars ->
                          ( _lhsOcopy,_lhsOisUnderscore,_lhsOpp,_lhsOpp',_lhsOstrictVars) }) }) }) }) }) })) )
-- Patterns ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         belowIrrefutable     : Bool
         isDeclOfLet          : Bool
         options              : Options
      synthesized attributes:
         copy                 : SELF 
         pps                  : [PP_Doc]
         pps'                 : [PP_Doc]
         strictVars           : [PP_Doc]
   alternatives:
      alternative Cons:
         child hd             : Pattern 
         child tl             : Patterns 
         visit 0:
            local copy        : _
      alternative Nil:
         visit 0:
            local copy        : _
-}
-- cata
sem_Patterns :: Patterns  ->
                T_Patterns 
sem_Patterns !list  =
    (Prelude.foldr sem_Patterns_Cons sem_Patterns_Nil (Prelude.map sem_Pattern list) )
-- semantic domain
newtype T_Patterns  = T_Patterns (Bool ->
                                  Bool ->
                                  Options ->
                                  ( Patterns,([PP_Doc]),([PP_Doc]),([PP_Doc])))
data Inh_Patterns  = Inh_Patterns {belowIrrefutable_Inh_Patterns :: !(Bool),isDeclOfLet_Inh_Patterns :: !(Bool),options_Inh_Patterns :: !(Options)}
data Syn_Patterns  = Syn_Patterns {copy_Syn_Patterns :: !(Patterns),pps_Syn_Patterns :: !([PP_Doc]),pps'_Syn_Patterns :: !([PP_Doc]),strictVars_Syn_Patterns :: !([PP_Doc])}
wrap_Patterns :: T_Patterns  ->
                 Inh_Patterns  ->
                 Syn_Patterns 
wrap_Patterns !(T_Patterns sem ) !(Inh_Patterns _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions )  =
    (let ( !_lhsOcopy,!_lhsOpps,!_lhsOpps',!_lhsOstrictVars) =
             (sem _lhsIbelowIrrefutable _lhsIisDeclOfLet _lhsIoptions )
     in  (Syn_Patterns _lhsOcopy _lhsOpps _lhsOpps' _lhsOstrictVars ))
sem_Patterns_Cons :: T_Pattern  ->
                     T_Patterns  ->
                     T_Patterns 
sem_Patterns_Cons !(T_Pattern hd_ ) !(T_Patterns tl_ )  =
    (T_Patterns (\ (!_lhsIbelowIrrefutable)
                   (!_lhsIisDeclOfLet)
                   (!_lhsIoptions) ->
                     (case (_lhsIoptions) of
                      { !_tlOoptions ->
                      (case (_lhsIisDeclOfLet) of
                       { !_tlOisDeclOfLet ->
                       (case (_lhsIbelowIrrefutable) of
                        { !_tlObelowIrrefutable ->
                        (case ((tl_ _tlObelowIrrefutable _tlOisDeclOfLet _tlOoptions )) of
                         { ( !_tlIcopy,!_tlIpps,!_tlIpps',!_tlIstrictVars) ->
                         (case (_lhsIoptions) of
                          { !_hdOoptions ->
                          (case (_lhsIisDeclOfLet) of
                           { !_hdOisDeclOfLet ->
                           (case (_lhsIbelowIrrefutable) of
                            { !_hdObelowIrrefutable ->
                            (case ((hd_ _hdObelowIrrefutable _hdOisDeclOfLet _hdOoptions )) of
                             { ( !_hdIcopy,!_hdIisUnderscore,!_hdIpp,!_hdIpp',!_hdIstrictVars) ->
                             (case ((:) _hdIcopy _tlIcopy) of
                              { !_copy ->
                              (case (_copy) of
                               { !_lhsOcopy ->
                               (case (_hdIpp : _tlIpps) of
                                { !_lhsOpps ->
                                (case (_hdIpp' : _tlIpps') of
                                 { !_lhsOpps' ->
                                 (case (_hdIstrictVars ++ _tlIstrictVars) of
                                  { !_lhsOstrictVars ->
                                  ( _lhsOcopy,_lhsOpps,_lhsOpps',_lhsOstrictVars) }) }) }) }) }) }) }) }) }) }) }) }) })) )
sem_Patterns_Nil :: T_Patterns 
sem_Patterns_Nil  =
    (T_Patterns (\ (!_lhsIbelowIrrefutable)
                   (!_lhsIisDeclOfLet)
                   (!_lhsIoptions) ->
                     (case ([]) of
                      { !_copy ->
                      (case (_copy) of
                       { !_lhsOcopy ->
                       (case ([]) of
                        { !_lhsOpps ->
                        (case ([]) of
                         { !_lhsOpps' ->
                         (case ([]) of
                          { !_lhsOstrictVars ->
                          ( _lhsOcopy,_lhsOpps,_lhsOpps',_lhsOstrictVars) }) }) }) }) })) )
-- Program -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         importBlocks         : PP_Doc
         mainFile             : String
         mainName             : String
         moduleHeader         : String -> String -> String -> Bool -> String
         options              : Options
         optionsLine          : String
         pragmaBlocks         : String
         textBlockMap         : Map BlockInfo PP_Doc
         textBlocks           : PP_Doc
      synthesized attributes:
         genIO                : IO ()
         output               : PP_Docs
   alternatives:
      alternative Program:
         child chunks         : Chunks 
         visit 0:
            local commonFile  : _
            local genCommonModule : _
            local mainModuleFile : _
            local genMainModule : _
-}
-- cata
sem_Program :: Program  ->
               T_Program 
sem_Program !(Program _chunks )  =
    (sem_Program_Program (sem_Chunks _chunks ) )
-- semantic domain
newtype T_Program  = T_Program (PP_Doc ->
                                String ->
                                String ->
                                (String -> String -> String -> Bool -> String) ->
                                Options ->
                                String ->
                                String ->
                                (Map BlockInfo PP_Doc) ->
                                PP_Doc ->
                                ( (IO ()),PP_Docs))
data Inh_Program  = Inh_Program {importBlocks_Inh_Program :: !(PP_Doc),mainFile_Inh_Program :: !(String),mainName_Inh_Program :: !(String),moduleHeader_Inh_Program :: !(String -> String -> String -> Bool -> String),options_Inh_Program :: !(Options),optionsLine_Inh_Program :: !(String),pragmaBlocks_Inh_Program :: !(String),textBlockMap_Inh_Program :: !(Map BlockInfo PP_Doc),textBlocks_Inh_Program :: !(PP_Doc)}
data Syn_Program  = Syn_Program {genIO_Syn_Program :: !(IO ()),output_Syn_Program :: !(PP_Docs)}
wrap_Program :: T_Program  ->
                Inh_Program  ->
                Syn_Program 
wrap_Program !(T_Program sem ) !(Inh_Program _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks )  =
    (let ( !_lhsOgenIO,!_lhsOoutput) =
             (sem _lhsIimportBlocks _lhsImainFile _lhsImainName _lhsImoduleHeader _lhsIoptions _lhsIoptionsLine _lhsIpragmaBlocks _lhsItextBlockMap _lhsItextBlocks )
     in  (Syn_Program _lhsOgenIO _lhsOoutput ))
sem_Program_Program :: T_Chunks  ->
                       T_Program 
sem_Program_Program !(T_Chunks chunks_ )  =
    (T_Program (\ (!_lhsIimportBlocks)
                  (!_lhsImainFile)
                  (!_lhsImainName)
                  (!_lhsImoduleHeader)
                  (!_lhsIoptions)
                  (!_lhsIoptionsLine)
                  (!_lhsIpragmaBlocks)
                  (!_lhsItextBlockMap)
                  (!_lhsItextBlocks) ->
                    (case (_lhsItextBlockMap) of
                     { !_chunksOtextBlockMap ->
                     (case (_lhsIpragmaBlocks) of
                      { !_chunksOpragmaBlocks ->
                      (case (_lhsIoptionsLine) of
                       { !_chunksOoptionsLine ->
                       (case (_lhsIoptions) of
                        { !_chunksOoptions ->
                        (case (_lhsImoduleHeader) of
                         { !_chunksOmoduleHeader ->
                         (case (_lhsImainName) of
                          { !_chunksOmainName ->
                          (case (_lhsImainFile) of
                           { !_chunksOmainFile ->
                           (case (_lhsImainFile ++ "_common.hs") of
                            { !_commonFile ->
                            (case (False) of
                             { !_chunksOisDeclOfLet ->
                             (case (nest _lhsIoptions) of
                              { !_chunksOnested ->
                              (case (_lhsItextBlocks) of
                               { !_chunksOtextBlocks ->
                               (case (_lhsIimportBlocks) of
                                { !_chunksOimportBlocks ->
                                (case ((chunks_ _chunksOimportBlocks _chunksOisDeclOfLet _chunksOmainFile _chunksOmainName _chunksOmoduleHeader _chunksOnested _chunksOoptions _chunksOoptionsLine _chunksOpragmaBlocks _chunksOtextBlockMap _chunksOtextBlocks )) of
                                 { ( !_chunksIappendCommon,!_chunksIappendMain,!_chunksIgenSems,!_chunksIimports,!_chunksIpps) ->
                                 (case (writeModule _commonFile
                                            ( [ pp $ _lhsIpragmaBlocks
                                              , pp $ _lhsIoptionsLine
                                              , pp $ _lhsImoduleHeader _lhsImainName "_common" "" True
                                              , _lhsIimportBlocks
                                              , _lhsItextBlocks
                                              ]
                                              ++ map vlist _chunksIappendCommon
                                            )) of
                                  { !_genCommonModule ->
                                  (case (_lhsImainFile ++ ".hs") of
                                   { !_mainModuleFile ->
                                   (case (writeModule _mainModuleFile
                                            ( [ pp $ _lhsIpragmaBlocks
                                              , pp $ _lhsIoptionsLine
                                              , pp $ _lhsImoduleHeader _lhsImainName "" "" False
                                              , pp $ ("import " ++ _lhsImainName ++ "_common\n")
                                              ]
                                              ++ map pp _chunksIimports
                                              ++ map vlist _chunksIappendMain
                                            )) of
                                    { !_genMainModule ->
                                    (case (do _genMainModule
                                              _genCommonModule
                                              _chunksIgenSems) of
                                     { !_lhsOgenIO ->
                                     (case (_chunksIpps) of
                                      { !_lhsOoutput ->
                                      ( _lhsOgenIO,_lhsOoutput) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) }) })) )
-- Type --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nested               : Bool
      synthesized attributes:
         pp                   : PP_Doc
         prec                 : Int
   alternatives:
      alternative Arr:
         child left           : Type 
         child right          : Type 
         visit 0:
            local r           : _
            local l           : _
      alternative CtxApp:
         child left           : {[(String, [String])]}
         child right          : Type 
      alternative List:
         child tp             : Type 
      alternative SimpleType:
         child txt            : {String}
      alternative TupleType:
         child tps            : Types 
      alternative TypeApp:
         child func           : Type 
         child args           : Types 
      alternative UnboxedTupleType:
         child tps            : Types 
-}
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type !(Arr _left _right )  =
    (sem_Type_Arr (sem_Type _left ) (sem_Type _right ) )
sem_Type !(CtxApp _left _right )  =
    (sem_Type_CtxApp _left (sem_Type _right ) )
sem_Type !(List _tp )  =
    (sem_Type_List (sem_Type _tp ) )
sem_Type !(SimpleType _txt )  =
    (sem_Type_SimpleType _txt )
sem_Type !(TupleType _tps )  =
    (sem_Type_TupleType (sem_Types _tps ) )
sem_Type !(TypeApp _func _args )  =
    (sem_Type_TypeApp (sem_Type _func ) (sem_Types _args ) )
sem_Type !(UnboxedTupleType _tps )  =
    (sem_Type_UnboxedTupleType (sem_Types _tps ) )
-- semantic domain
newtype T_Type  = T_Type (Bool ->
                          ( PP_Doc,Int))
data Inh_Type  = Inh_Type {nested_Inh_Type :: !(Bool)}
data Syn_Type  = Syn_Type {pp_Syn_Type :: !(PP_Doc),prec_Syn_Type :: !(Int)}
wrap_Type :: T_Type  ->
             Inh_Type  ->
             Syn_Type 
wrap_Type !(T_Type sem ) !(Inh_Type _lhsInested )  =
    (let ( !_lhsOpp,!_lhsOprec) =
             (sem _lhsInested )
     in  (Syn_Type _lhsOpp _lhsOprec ))
sem_Type_Arr :: T_Type  ->
                T_Type  ->
                T_Type 
sem_Type_Arr !(T_Type left_ ) !(T_Type right_ )  =
    (T_Type (\ (!_lhsInested) ->
                 (case (_lhsInested) of
                  { !_rightOnested ->
                  (case (_lhsInested) of
                   { !_leftOnested ->
                   (case ((right_ _rightOnested )) of
                    { ( !_rightIpp,!_rightIprec) ->
                    (case (if _rightIprec <  2 then pp_parens _rightIpp else _rightIpp) of
                     { !_r ->
                     (case ((left_ _leftOnested )) of
                      { ( !_leftIpp,!_leftIprec) ->
                      (case (if _leftIprec  <= 2 then pp_parens _leftIpp  else _leftIpp) of
                       { !_l ->
                       (case (_l     >#< "->" >-< _r) of
                        { !_lhsOpp ->
                        (case (2) of
                         { !_lhsOprec ->
                         ( _lhsOpp,_lhsOprec) }) }) }) }) }) }) }) })) )
sem_Type_CtxApp :: ([(String, [String])]) ->
                   T_Type  ->
                   T_Type 
sem_Type_CtxApp !left_ !(T_Type right_ )  =
    (T_Type (\ (!_lhsInested) ->
                 (case (_lhsInested) of
                  { !_rightOnested ->
                  (case ((right_ _rightOnested )) of
                   { ( !_rightIpp,!_rightIprec) ->
                   (case ((pp_block "(" ")" "," $ map (\(n,ns) -> hv_sp $ map pp (n:ns)) left_) >#< "=>" >#< _rightIpp) of
                    { !_lhsOpp ->
                    (case (_rightIprec) of
                     { !_lhsOprec ->
                     ( _lhsOpp,_lhsOprec) }) }) }) })) )
sem_Type_List :: T_Type  ->
                 T_Type 
sem_Type_List !(T_Type tp_ )  =
    (T_Type (\ (!_lhsInested) ->
                 (case (_lhsInested) of
                  { !_tpOnested ->
                  (case ((tp_ _tpOnested )) of
                   { ( !_tpIpp,!_tpIprec) ->
                   (case ("[" >|< _tpIpp >|< "]") of
                    { !_lhsOpp ->
                    (case (5) of
                     { !_lhsOprec ->
                     ( _lhsOpp,_lhsOprec) }) }) }) })) )
sem_Type_SimpleType :: String ->
                       T_Type 
sem_Type_SimpleType !txt_  =
    (T_Type (\ (!_lhsInested) ->
                 (case (if reallySimple txt_ then text txt_ else pp_parens (text txt_)) of
                  { !_lhsOpp ->
                  (case (5) of
                   { !_lhsOprec ->
                   ( _lhsOpp,_lhsOprec) }) })) )
sem_Type_TupleType :: T_Types  ->
                      T_Type 
sem_Type_TupleType !(T_Types tps_ )  =
    (T_Type (\ (!_lhsInested) ->
                 (case (_lhsInested) of
                  { !_tpsOnested ->
                  (case ((tps_ _tpsOnested )) of
                   { ( !_tpsIpps) ->
                   (case (ppTuple _lhsInested _tpsIpps) of
                    { !_lhsOpp ->
                    (case (5) of
                     { !_lhsOprec ->
                     ( _lhsOpp,_lhsOprec) }) }) }) })) )
sem_Type_TypeApp :: T_Type  ->
                    T_Types  ->
                    T_Type 
sem_Type_TypeApp !(T_Type func_ ) !(T_Types args_ )  =
    (T_Type (\ (!_lhsInested) ->
                 (case (_lhsInested) of
                  { !_argsOnested ->
                  (case (_lhsInested) of
                   { !_funcOnested ->
                   (case ((args_ _argsOnested )) of
                    { ( !_argsIpps) ->
                    (case ((func_ _funcOnested )) of
                     { ( !_funcIpp,!_funcIprec) ->
                     (case (hv_sp (_funcIpp : _argsIpps)) of
                      { !_lhsOpp ->
                      (case (_funcIprec) of
                       { !_lhsOprec ->
                       ( _lhsOpp,_lhsOprec) }) }) }) }) }) })) )
sem_Type_UnboxedTupleType :: T_Types  ->
                             T_Type 
sem_Type_UnboxedTupleType !(T_Types tps_ )  =
    (T_Type (\ (!_lhsInested) ->
                 (case (_lhsInested) of
                  { !_tpsOnested ->
                  (case ((tps_ _tpsOnested )) of
                   { ( !_tpsIpps) ->
                   (case (ppUnboxedTuple _lhsInested _tpsIpps) of
                    { !_lhsOpp ->
                    (case (5) of
                     { !_lhsOprec ->
                     ( _lhsOpp,_lhsOprec) }) }) }) })) )
-- Types -------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         nested               : Bool
      synthesized attribute:
         pps                  : PP_Docs
   alternatives:
      alternative Cons:
         child hd             : Type 
         child tl             : Types 
      alternative Nil:
-}
-- cata
sem_Types :: Types  ->
             T_Types 
sem_Types !list  =
    (Prelude.foldr sem_Types_Cons sem_Types_Nil (Prelude.map sem_Type list) )
-- semantic domain
newtype T_Types  = T_Types (Bool ->
                            ( PP_Docs))
data Inh_Types  = Inh_Types {nested_Inh_Types :: !(Bool)}
data Syn_Types  = Syn_Types {pps_Syn_Types :: !(PP_Docs)}
wrap_Types :: T_Types  ->
              Inh_Types  ->
              Syn_Types 
wrap_Types !(T_Types sem ) !(Inh_Types _lhsInested )  =
    (let ( !_lhsOpps) =
             (sem _lhsInested )
     in  (Syn_Types _lhsOpps ))
sem_Types_Cons :: T_Type  ->
                  T_Types  ->
                  T_Types 
sem_Types_Cons !(T_Type hd_ ) !(T_Types tl_ )  =
    (T_Types (\ (!_lhsInested) ->
                  (case (_lhsInested) of
                   { !_tlOnested ->
                   (case (_lhsInested) of
                    { !_hdOnested ->
                    (case ((tl_ _tlOnested )) of
                     { ( !_tlIpps) ->
                     (case ((hd_ _hdOnested )) of
                      { ( !_hdIpp,!_hdIprec) ->
                      (case (_hdIpp : _tlIpps) of
                       { !_lhsOpps ->
                       ( _lhsOpps) }) }) }) }) })) )
sem_Types_Nil :: T_Types 
sem_Types_Nil  =
    (T_Types (\ (!_lhsInested) ->
                  (case ([]) of
                   { !_lhsOpps ->
                   ( _lhsOpps) })) )