

-- UUAGC 0.9.10 (PrintErrorMessages.ag)
module PrintErrorMessages where

import Pretty
import UU.Scanner.Position(Pos(..), noPos)
import ErrorMessages
import DepTypes
import Options
import Data.List(mapAccumL,intersect,(\\))
import GrammarInfo


import UU.Scanner.Position(Pos)
import Pretty
import DepTypes(Trace)
import CodeSyntax
import CommonTypes

isError :: Options -> Error -> Bool
isError opts (ParserError     _ _ _    ) = True
isError opts (DupAlt          _ _ _    ) = False
isError opts (DupSynonym      _ _      ) = False
isError opts (DupSet          _ _      ) = False
isError opts (DupInhAttr      _ _ _    ) = True
isError opts (DupSynAttr      _ _ _    ) = True
isError opts (DupChild        _ _ _ _  ) = False
isError opts (DupRule         _ _ _ _ _) = True
isError opts (DupSig          _ _ _    ) = False
isError opts (UndefNont       _        ) = True
isError opts (UndefAlt        _ _      ) = True
isError opts (UndefChild      _ _ _    ) = True
isError opts (MissingRule     _ _ _ _  ) = False
isError opts (SuperfluousRule _ _ _ _  ) = False
isError opts (UndefLocal      _ _ _    ) = True
isError opts (ChildAsLocal    _ _ _    ) = False
isError opts (UndefAttr       _ _ _ _ _) = True
isError opts (CyclicSet       _        ) = True
isError opts (CustomError     w _ _    ) = not w
isError opts (LocalCirc       _ _ _ _ _) = cycleIsDangerous opts
isError opts (InstCirc        _ _ _ _ _) = cycleIsDangerous opts
isError opts (DirectCirc      _ _ _    ) = cycleIsDangerous opts
isError opts (InducedCirc     _ _ _    ) = cycleIsDangerous opts
isError opts (MissingTypeSig  _ _ _    ) = False
isError opts (MissingInstSig  _ _ _    ) = True
isError opts (DupUnique       _ _ _    ) = False
isError opts (MissingUnique   _ _      ) = True

cycleIsDangerous :: Options -> Bool
cycleIsDangerous opts
  = any ($ opts) [ wignore, bangpats, cases, strictCases, stricterCases, strictSems, withCycle ]


toWidth n xs | k<n       = xs ++ replicate (n-k) ' '
             | otherwise = xs
               where k = length xs
	
showEdge ((inh,syn),_,_) 
  = text ("inherited attribute " ++ toWidth 20 (getName inh) ++ " with synthesized attribute " ++  getName syn)

showEdgeLong ((inh,syn),path1,path2) 
  = text ("inherited attribute " ++ getName inh ++ " is needed for " ++  "synthesized attribute " ++ getName syn)
    >-< indent 4 (vlist (map text path2))
    >-< text "and back: "
    >-< indent 4 (vlist (map text path1))
	
attrText inh syn
 = 	if   inh == syn 
    then "threaded attribute " ++ getName inh
    else "inherited attribute " ++ getName inh ++ " and synthesized attribute " ++getName syn	

        
ppTrace :: Trace -> PP_Doc
ppTrace (tr:rest) = wfill [showLineNr (lineNr tr) ++ ":", "SEM", getName (nt tr), "|", getName (prod tr)
                         , lhsFld tr +.+ lhsAttr tr, "=", "..." +#+ showRhs tr +#+ "..."]
                    >-< ppTrace rest
ppTrace [] = empty

showRhs :: TraceElem -> String
showRhs tr = if null (getName field)
             then "@" ++ getName attr
             else "@" ++ (field +.+ attr)
 where field = rhsFld tr
       attr  = rhsAttr tr
 
showLineNr :: Int -> String
showLineNr i | i==(-1) = "CR"
             | otherwise = show i

showAttrDef f a | f == _LHS  = "synthesized attribute " ++ getName a
                | f == _LOC  = "local attribute " ++ getName a
                | f == _INST = "inst attribute " ++ getName a
                | otherwise  = "inherited attribute " ++ getName a ++ " of field " ++ getName f

showAttrUse f a | f == _LHS  = "inherited attribute " ++ getName a
                | f == _LOC  = "local attribute " ++ getName a 
                | f == _INST = "inst attribute " ++ getName a
                | otherwise  = "synthesized attribute " ++ getName a ++ " of field " ++ getName f

ppAttr f a = text (getName f++"."++getName a)
ppAttrUse f a = "@" >|< ppAttr f a


infixr 5 +#+
(+#+) :: String -> String -> String
(+#+) s t = s ++ " " ++ t

infixr 5 +.+
(+.+) :: Identifier -> Identifier -> String
(+.+) s t = getName s ++ "." ++ getName t

wfill = fill . addSpaces. concat . map words
  where addSpaces (x:xs) = x:map addSpace xs
        addSpaces []     = []
        addSpace  [x]    | x `elem` ".,;:!?" = [x]
        addSpace  xs     = ' ':xs

ppError :: Bool           -- class of the error, True:error False:warning
        -> Pos      -- source position
        -> PP_Doc         -- error message
        -> PP_Doc         -- pattern
        -> PP_Doc         -- help, more info
        -> PP_Doc         -- action taken by AG
        -> Bool           -- verbose? show help and action?
        -> PP_Doc
ppError isError pos mesg pat hlp act verbose
  = let position = case pos of
                     Pos l c f | l >= 0    -> f >|< ":" >|< show l >|< ":" >|< show c
                               | otherwise -> pp "uuagc"
        tp      = if isError then "error" else "warning"
        header  = position >|< ":" >#< tp >|< ":" >#< mesg
        pattern = "pattern  :" >#< pat
        help    = "help     :" >#< hlp
        action  = "action   :" >#< act
    in if verbose
         then vlist [text "",header,pattern,help,action]
         else header

{-
-- old error reporting code
  = let 
      cl = if isError then "ERROR" else "Warning"
      position   = case pos of
                         (Pos l c f) | l >= 0    -> f >|< ": line " >|< show l >|< ", column " >|< show c
                                     | otherwise -> empty
      header     = "*** UU.AG" >#< cl >#< position >#< "***"
      message    = "problem  :" >#< mesg
      pattern    = "pattern  :" >#< pat
      help       = "help     :" >#< hlp
      action     = "action   :" >#< act
    in
      if verbose
         then vlist [text "",header,message,pattern,help,action]
         else vlist [text "",header,message]
-}

showPos = show . getPos

ppInterface inter = wfill ["interface:", show inter]

-- Error -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         options              : Options
         verbose              : Bool
      synthesized attributes:
         me                   : SELF 
         pp                   : PP_Doc
   alternatives:
      alternative ChildAsLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
         visit 0:
            local me          : _
      alternative CustomError:
         child isWarning      : {Bool}
         child pos            : {Pos}
         child mesg           : {PP_Doc}
         visit 0:
            local me          : _
      alternative CyclicSet:
         child name           : {Identifier}
         visit 0:
            local me          : _
      alternative DirectCirc:
         child nt             : {NontermIdent}
         child o_visit        : {Bool}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
         visit 0:
            local me          : _
      alternative DupAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child occ1           : {ConstructorIdent}
         visit 0:
            local me          : _
      alternative DupChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupInhAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupSet:
         child name           : {NontermIdent}
         child occ1           : {NontermIdent}
         visit 0:
            local me          : _
      alternative DupSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative DupSynAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupSynonym:
         child nt             : {NontermIdent}
         child occ1           : {NontermIdent}
         visit 0:
            local me          : _
      alternative DupUnique:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative InducedCirc:
         child nt             : {NontermIdent}
         child cinter         : {CInterface}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
         visit 0:
            local me          : _
      alternative InstCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
         visit 0:
            local me          : _
      alternative LocalCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
         visit 0:
            local me          : _
      alternative MissingInstSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingTypeSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingUnique:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative ParserError:
         child pos            : {Pos}
         child problem        : {String}
         child action         : {String}
         visit 0:
            local me          : _
      alternative SuperfluousRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative UndefAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         visit 0:
            local me          : _
      alternative UndefAttr:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child isOut          : {Bool}
         visit 0:
            local me          : _
      alternative UndefChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
         visit 0:
            local me          : _
      alternative UndefLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
         visit 0:
            local me          : _
      alternative UndefNont:
         child nt             : {NontermIdent}
         visit 0:
            local me          : _
-}
-- cata
sem_Error :: Error  ->
             T_Error 
sem_Error (ChildAsLocal _nt _con _var )  =
    (sem_Error_ChildAsLocal _nt _con _var )
sem_Error (CustomError _isWarning _pos _mesg )  =
    (sem_Error_CustomError _isWarning _pos _mesg )
sem_Error (CyclicSet _name )  =
    (sem_Error_CyclicSet _name )
sem_Error (DirectCirc _nt _o_visit _cyclic )  =
    (sem_Error_DirectCirc _nt _o_visit _cyclic )
sem_Error (DupAlt _nt _con _occ1 )  =
    (sem_Error_DupAlt _nt _con _occ1 )
sem_Error (DupChild _nt _con _name _occ1 )  =
    (sem_Error_DupChild _nt _con _name _occ1 )
sem_Error (DupInhAttr _nt _attr _occ1 )  =
    (sem_Error_DupInhAttr _nt _attr _occ1 )
sem_Error (DupRule _nt _con _field _attr _occ1 )  =
    (sem_Error_DupRule _nt _con _field _attr _occ1 )
sem_Error (DupSet _name _occ1 )  =
    (sem_Error_DupSet _name _occ1 )
sem_Error (DupSig _nt _con _attr )  =
    (sem_Error_DupSig _nt _con _attr )
sem_Error (DupSynAttr _nt _attr _occ1 )  =
    (sem_Error_DupSynAttr _nt _attr _occ1 )
sem_Error (DupSynonym _nt _occ1 )  =
    (sem_Error_DupSynonym _nt _occ1 )
sem_Error (DupUnique _nt _con _attr )  =
    (sem_Error_DupUnique _nt _con _attr )
sem_Error (InducedCirc _nt _cinter _cyclic )  =
    (sem_Error_InducedCirc _nt _cinter _cyclic )
sem_Error (InstCirc _nt _con _attr _o_visit _path )  =
    (sem_Error_InstCirc _nt _con _attr _o_visit _path )
sem_Error (LocalCirc _nt _con _attr _o_visit _path )  =
    (sem_Error_LocalCirc _nt _con _attr _o_visit _path )
sem_Error (MissingInstSig _nt _con _attr )  =
    (sem_Error_MissingInstSig _nt _con _attr )
sem_Error (MissingRule _nt _con _field _attr )  =
    (sem_Error_MissingRule _nt _con _field _attr )
sem_Error (MissingTypeSig _nt _con _attr )  =
    (sem_Error_MissingTypeSig _nt _con _attr )
sem_Error (MissingUnique _nt _attr )  =
    (sem_Error_MissingUnique _nt _attr )
sem_Error (ParserError _pos _problem _action )  =
    (sem_Error_ParserError _pos _problem _action )
sem_Error (SuperfluousRule _nt _con _field _attr )  =
    (sem_Error_SuperfluousRule _nt _con _field _attr )
sem_Error (UndefAlt _nt _con )  =
    (sem_Error_UndefAlt _nt _con )
sem_Error (UndefAttr _nt _con _field _attr _isOut )  =
    (sem_Error_UndefAttr _nt _con _field _attr _isOut )
sem_Error (UndefChild _nt _con _name )  =
    (sem_Error_UndefChild _nt _con _name )
sem_Error (UndefLocal _nt _con _var )  =
    (sem_Error_UndefLocal _nt _con _var )
sem_Error (UndefNont _nt )  =
    (sem_Error_UndefNont _nt )
-- semantic domain
newtype T_Error  = T_Error (Options ->
                            Bool ->
                            ( Error,PP_Doc))
data Inh_Error  = Inh_Error {options_Inh_Error :: Options,verbose_Inh_Error :: Bool}
data Syn_Error  = Syn_Error {me_Syn_Error :: Error,pp_Syn_Error :: PP_Doc}
wrap_Error :: T_Error  ->
              Inh_Error  ->
              Syn_Error 
wrap_Error (T_Error sem ) (Inh_Error _lhsIoptions _lhsIverbose )  =
    (let ( _lhsOme,_lhsOpp) =
             (sem _lhsIoptions _lhsIverbose )
     in  (Syn_Error _lhsOme _lhsOpp ))
sem_Error_ChildAsLocal :: NontermIdent ->
                          ConstructorIdent ->
                          Identifier ->
                          T_Error 
sem_Error_ChildAsLocal nt_ con_ var_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 302, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Nontrivial field ",getName var_, "is used as local at constructor"
                                             , getName con_ , "of nonterminal",getName nt_, "."
                                             ]
                               pat   = "SEM" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< "... = "
                                                       >#< "..." >#< "@" >|< getName var_ >#< "..." )
                               help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                             , getName nt_ , "contains a nontrivial field name", getName var_, "."
                                             ,"You should use @", getName var_, ".self instead, where self is a SELF-attribute."
                                             ]
                               act  = wfill ["The generated program probably contains a type error or has undefined variables."]
                           in ppError (isError _lhsIoptions _me) (getPos var_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           ChildAsLocal nt_ con_ var_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_CustomError :: Bool ->
                         Pos ->
                         PP_Doc ->
                         T_Error 
sem_Error_CustomError isWarning_ pos_ mesg_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 344, column 21)
                       _lhsOpp =
                           let pat   =  text "unknown"
                               help = wfill ["not available."]
                               act  = wfill ["unknown"]
                           in ppError (isError _lhsIoptions _me) pos_ mesg_ pat help act False
                       -- self rule
                       _me =
                           CustomError isWarning_ pos_ mesg_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_CyclicSet :: Identifier ->
                       T_Error 
sem_Error_CyclicSet name_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 335, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Cyclic definition for nonterminal set", getName name_]
                               pat   = "SET" >#< getName name_ >#< "=" >#< "..." >#< getName name_ >#< "..."
                               help =  wfill ["The defintion for a nonterminal set named" , getName name_
                                             ,"directly or indirectly refers to itself."
                                             ,"Adapt the definition of the nonterminal set, to remove the cyclic dependency."
                                             ]
                               act  = wfill ["The nonterminal set", getName name_, "is considered to be empty."]
                           in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           CyclicSet name_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DirectCirc :: NontermIdent ->
                        Bool ->
                        ([((Identifier,Identifier),[String],[String])]) ->
                        T_Error 
sem_Error_DirectCirc nt_ o_visit_ cyclic_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 373, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["In nonterminal", getName nt_, "synthesized and inherited attributes are mutually dependent" ]
                                       >-< vlist (map showEdge cyclic_)
                               pat   = text ""
                               help  = vlist (map showEdgeLong cyclic_)
                               act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                     | otherwise = text "The generated program might hang when run."
                           in ppError (isError _lhsIoptions _me) noPos mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DirectCirc nt_ o_visit_ cyclic_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupAlt :: NontermIdent ->
                    ConstructorIdent ->
                    ConstructorIdent ->
                    T_Error 
sem_Error_DupAlt nt_ con_ occ1_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 75, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Repeated definition for alternative", getName con_
                                             ,"of nonterminal", getName nt_, "."
                                             ] >-<
                                       wfill ["First definition:", (showPos occ1_),"."] >-<
                                       wfill ["Other definition:", (showPos con_),"."]
                               pat =     "DATA" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< "...")
                                     >-< indent 2 ("|" >#< getName con_ >#< "...")
                               help =  wfill ["The nonterminal",getName nt_,"has more than one alternative that"
                                             ,"is labelled with the constructor name",getName con_,"."
                                             ,"You should either rename or remove enough of them to make all"
                                             ,"constructors of",getName nt_,"uniquely named."
                                             ]
                               act  = wfill [ "The first alternative of name",getName con_
                                            ,"you have given for nonterminal",getName nt_
                                            ,"is considered valid. All other alternatives have been discarded."
                                            ]
                           in ppError (isError _lhsIoptions _me) (getPos con_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupAlt nt_ con_ occ1_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupChild :: NontermIdent ->
                      ConstructorIdent ->
                      Identifier ->
                      Identifier ->
                      T_Error 
sem_Error_DupChild nt_ con_ name_ occ1_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 168, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Repeated declaration for field", getName name_, "of alternative"
                                             ,getName con_, "of nonterminal", getName nt_, "."
                                             ] >-<
                                       wfill ["First definition:", (showPos occ1_),"."] >-<
                                       wfill ["Other definition:", (showPos name_),"."]
                               pat   =   "DATA" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< (getName name_ >|< ":..." >-< getName name_ >|< ":..."))
                               help =  wfill ["The alternative" ,getName con_ , "of nonterminal" ,getName nt_
                                             ,"has more than one field that is named"
                                             , getName name_ ++ ". Possibly they have different types."
                                             ,"You should either rename or remove enough of them to make all fields of"
                                             ,getName con_ , "for nonterminal " , getName nt_ , "uniquely named."
                                             ]
                               act  = wfill ["The last declaration with its corresponding type is considered valid."
                                            ,"All others have been discarded."
                                            ]
                           in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupChild nt_ con_ name_ occ1_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupInhAttr :: NontermIdent ->
                        Identifier ->
                        Identifier ->
                        T_Error 
sem_Error_DupInhAttr nt_ attr_ occ1_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 130, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Repeated declaration of inherited attribute", getName attr_
                                             , "of nonterminal", getName nt_, "."
                                             ] >-<
                                       wfill ["First definition:", (showPos occ1_),"."] >-<
                                       wfill ["Other definition:", (showPos attr_),"."]
                               pat  = "ATTR" >#< getName nt_ >#< "[" >#< getName attr_ >|< ":...,"
                                                             >#< getName attr_ >|< ":... | | ]"
                               help =  wfill ["The identifier" , getName attr_ ,"has been declared"
                                             ,"as an inherited (or chained) attribute for nonterminal"
                                             ,getName nt_ , "more than once, with possibly different types."
                                             ,"Delete all but one or rename them to make them unique."
                                             ]
                               act  = wfill ["One declaration with its corresponding type is considered valid."
                                            ,"All others have been discarded. The generated program will probably not run."
                                            ]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupInhAttr nt_ attr_ occ1_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupRule :: NontermIdent ->
                     ConstructorIdent ->
                     Identifier ->
                     Identifier ->
                     Identifier ->
                     T_Error 
sem_Error_DupRule nt_ con_ field_ attr_ occ1_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 188, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more rules for"
                                             ,showAttrDef field_ attr_,"."
                                             ]  >-<
                                       wfill ["First rule:", (showPos occ1_),"."] >-<
                                       wfill ["Other rule:", (showPos attr_),"."]
                               pat   =   "SEM" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                               help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                     ,", there is more than one rule for the" , showAttrDef field_ attr_
                                                     ,". You should either rename or remove enough of them to make all rules for alternative"
                                                     ,getName con_ , "of nonterminal " ,getName  nt_ , "uniquely named."
                                                     ]
                               act  = wfill ["The last rule given is considered valid. All others have been discarded."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupRule nt_ con_ field_ attr_ occ1_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupSet :: NontermIdent ->
                    NontermIdent ->
                    T_Error 
sem_Error_DupSet name_ occ1_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 114, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Definition of nonterminal set", getName name_, "clashes with another"
                                             ,"set, a type synonym or a data definition."
                                             ] >-<
                                       wfill ["First definition:", (showPos occ1_),"."] >-<
                                       wfill ["Set definition:"   , (showPos name_),"."]
                               pat =     "SET" >#< getName name_ >#< "=" >#<  "..."
                                     >-< "SET" >#< getName name_ >#< "=" >#<  "..."
                               help =  wfill ["A nonterminal set with name", getName  name_
                                             ,"has been given while there already is a SET, DATA, or TYPE"
                                             ,"definition with the same name."
                                             ,"You should either rename or remove the nonterminal set."
                                             ]
                               act  = wfill [ "The clashing nonterminal set will be ignored."
                                            ]
                           in ppError (isError _lhsIoptions _me)  (getPos name_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupSet name_ occ1_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupSig :: NontermIdent ->
                    ConstructorIdent ->
                    Identifier ->
                    T_Error 
sem_Error_DupSig nt_ con_ attr_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 206, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more typesignatures for"
                                             ,showAttrDef _LOC attr_,"."
                                             ]  >-<
                                       wfill ["First signature:", (showPos attr_),"."]
                               pat   =   "SEM" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< "= ...")
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< "= ...")
                               help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                     ,", there is more than one rule for the" , showAttrDef _LOC attr_
                                                     ,". You should remove enough of them to make all typesignatures for alternative"
                                                     ,getName con_ , "of nonterminal " ,getName  nt_ , "unique."
                                                     ]
                               act  = wfill ["The last typesignature given is considered valid. All others have been discarded."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupSig nt_ con_ attr_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupSynAttr :: NontermIdent ->
                        Identifier ->
                        Identifier ->
                        T_Error 
sem_Error_DupSynAttr nt_ attr_ occ1_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 149, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Repeated declaration of synthesized attribute", getName attr_
                                             , "of nonterminal", getName nt_, "."
                                             ] >-<
                                       wfill ["First definition:", (showPos occ1_),"."] >-<
                                       wfill ["Other definition:", (showPos attr_),"."]
                               pat  = "ATTR" >#< getName nt_ >#< "[ | |" >#< getName attr_ >|< ":...,"
                                                                 >#< getName attr_ >|< ":... ]"
                               help =  wfill ["The identifier" , getName attr_ ,"has been declared"
                                             ,"as a synthesized (or chained) attribute for nonterminal"
                                             ,getName nt_ , "more than once, with possibly different types."
                                             ,"Delete all but one or rename them to make them unique."
                                             ]
                               act  = wfill ["One declaration with its corresponding type is considered valid."
                                            ,"All others have been discarded. The generated program will probably not run."
                                            ]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupSynAttr nt_ attr_ occ1_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupSynonym :: NontermIdent ->
                        NontermIdent ->
                        T_Error 
sem_Error_DupSynonym nt_ occ1_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 97, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Definition of type synonym", getName nt_, "clashes with another"
                                             ,"type synonym."
                                             ] >-<
                                       wfill ["First definition:", (showPos occ1_),"."] >-<
                                       wfill ["Type synonym :"   , (showPos nt_),"."]
                               pat =     "DATA" >#< getName nt_
                                     >-< indent 2 ("|" >#< "...")
                                     >-< "TYPE" >#< getName nt_ >#< "=" >#<  "..."
                               help =  wfill ["A type synonym with name", getName  nt_
                                             ,"has been given while there already is TYPE"
                                             ,"definition with the same name."
                                             ,"You should either rename or remove the type synonym."
                                             ]
                               act  = wfill [ "The clashing type synonym will be ignored."
                                            ]
                           in ppError (isError _lhsIoptions _me)  (getPos nt_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupSynonym nt_ occ1_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_DupUnique :: NontermIdent ->
                       ConstructorIdent ->
                       Identifier ->
                       T_Error 
sem_Error_DupUnique nt_ con_ attr_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 434, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more unique-attribute signatures for"
                                             ,showAttrDef _LOC attr_,"."
                                             ]  >-<
                                       wfill ["First signature:", (showPos attr_),"."]
                               pat   =   "SEM" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< " : UNIQUEREF ...")
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< " : UNIQUEREF ...")
                               help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                     ,", there is more than one unique-attribute signature for the" , showAttrDef _LOC attr_
                                                     ,". You should remove enough of them to make all unique-signatures for alternative"
                                                     ,getName con_ , "of nonterminal " ,getName  nt_ , "unique."
                                                     ]
                               act  = wfill ["Unpredicatable sharing of unique numbers may occur."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           DupUnique nt_ con_ attr_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_InducedCirc :: NontermIdent ->
                         CInterface ->
                         ([((Identifier,Identifier),[String],[String])]) ->
                         T_Error 
sem_Error_InducedCirc nt_ cinter_ cyclic_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 381, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["After scheduling, in nonterminal", getName nt_, "synthesized and inherited attributes have an INDUCED mutual dependency" ]
                                       >-< vlist (map showEdge cyclic_)
                               pat   = text ""
                               showInter (CInterface segs) = concat (snd (mapAccumL (\i c -> (i+1,("visit " ++ show i) : map ind (showsSegment c))) 0 segs))
                               help  = vlist (("Interface for nonterminal " ++ getName nt_ ++ ":") : map ind (showInter cinter_))
                                       >-< vlist (map showEdgeLong cyclic_)
                               act   = text "An unoptimized version was generated. It might hang when run."
                           in ppError (isError _lhsIoptions _me) noPos mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           InducedCirc nt_ cinter_ cyclic_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_InstCirc :: NontermIdent ->
                      ConstructorIdent ->
                      Identifier ->
                      Bool ->
                      ([String]) ->
                      T_Error 
sem_Error_InstCirc nt_ con_ attr_ o_visit_ path_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 361, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Circular dependency for inst attribute", getName attr_
                                             , "of alternative", getName con_, "of nonterminal", getName nt_]
                               pat   = "SEM" >#< getName nt_
                                       >-< indent 2 ("|" >#< getName con_ >#< "inst." >|< getName attr_ >#< "="
                                                         >#< "..." >#< "@s.<some attribte>" >#< "...")
                               help  = if null path_
                                       then text "the definition is directly circular"
                                       else hlist ("The following attributes are involved in the cycle:": path_)
                               act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                     | otherwise = text "The generated program might hang when run."
                           in ppError (isError _lhsIoptions _me) (getPos (attr_)) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           InstCirc nt_ con_ attr_ o_visit_ path_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_LocalCirc :: NontermIdent ->
                       ConstructorIdent ->
                       Identifier ->
                       Bool ->
                       ([String]) ->
                       T_Error 
sem_Error_LocalCirc nt_ con_ attr_ o_visit_ path_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 349, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Circular dependency for local attribute", getName attr_
                                             , "of alternative", getName con_, "of nonterminal", getName nt_]
                               pat   = "SEM" >#< getName nt_
                                       >-< indent 2 ("|" >#< getName con_ >#< "loc." >|< getName attr_ >#< "="
                                                         >#< "..." >#< "@loc." >|< getName attr_ >#< "...")
                               help  = if null path_
                                       then text "the definition is directly circular"
                                       else hlist ("The following attributes are involved in the cycle:": path_)
                               act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                     | otherwise = text "The generated program might hang when run."
                           in ppError (isError _lhsIoptions _me) (getPos (attr_)) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           LocalCirc nt_ con_ attr_ o_visit_ path_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_MissingInstSig :: NontermIdent ->
                            ConstructorIdent ->
                            Identifier ->
                            T_Error 
sem_Error_MissingInstSig nt_ con_ attr_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 404, column 21)
                       _lhsOpp =
                           let mesg = wfill ["Type signature needed, but not found for", showAttrDef _INST attr_ , "in alternative"
                                             , getName con_ , "of nonterminal",getName nt_ ,"."
                                             ]>-<
                                       wfill ["Location:", (showPos attr_),"."]
                               pat   = "SEM" >#< nt_
                                         >-< indent 2 ("|" >#< getName con_ >#< ppAttr _INST attr_ >#< ": ...")
                               help  = wfill ["The", showAttrDef _INST attr_, "in alternative", getName con_
                                             ,"of nonterminal", getName nt_, "is a non-terminal attribute, so "
                                             ,"its type is needed to attribute its value."
                                             ,"Please supply its type."
                                             ]
                               act  = wfill ["It is not possible to proceed without this signature."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           MissingInstSig nt_ con_ attr_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_MissingRule :: NontermIdent ->
                         ConstructorIdent ->
                         Identifier ->
                         Identifier ->
                         T_Error 
sem_Error_MissingRule nt_ con_ field_ attr_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 260, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Missing rule for", showAttrDef field_ attr_ , "in alternative"
                                             , getName con_ , "of nonterminal",getName nt_ ,"."
                                             ]
                               pat   =   "SEM" >#< nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                               help  = wfill ["The", showAttrDef field_ attr_, "in alternative", getName con_
                                             , "of nonterminal", getName nt_, "is missing and cannot be inferred"
                                             ,"by a copy rule, so you should add an appropriate rule."
                                             ]
                               act  = wfill ["The value of the attribute has been set to undefined."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           MissingRule nt_ con_ field_ attr_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_MissingTypeSig :: NontermIdent ->
                            ConstructorIdent ->
                            Identifier ->
                            T_Error 
sem_Error_MissingTypeSig nt_ con_ attr_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 390, column 21)
                       _lhsOpp =
                           let mesg = wfill ["Type signature needed, but not found for", showAttrDef _LOC attr_ , "in alternative"
                                             , getName con_ , "of nonterminal",getName nt_ ,"."
                                             ]>-<
                                       wfill ["Location:", (showPos attr_),"."]
                               pat   =   "SEM" >#< nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< ": ...")
                               help  = wfill ["The", showAttrDef _LOC attr_, "in alternative", getName con_
                                             ,"of nonterminal", getName nt_, "is needed in two separate visits to", getName nt_
                                             ,"so its type is needed to generate type signatures."
                                             ,"Please supply its type."
                                             ]
                               act  = wfill ["The type signatures of semantic functions are not generated."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           MissingTypeSig nt_ con_ attr_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_MissingUnique :: NontermIdent ->
                           Identifier ->
                           T_Error 
sem_Error_MissingUnique nt_ attr_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 418, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Missing unique counter (chained attribute)"
                                             , getName attr_
                                             , "at nonterminal"
                                             , getName nt_, "."
                                             ]
                               pat   = "ATTR" >#< getName nt_ >#< "[ |" >#< getName attr_ >#< " : ... | ]"
                               help =  wfill ["A unique attribute signature in a constructor for nonterminal" , getName nt_
                                             , "refers to an unique counter (chained attribute) named "
                                             , getName attr_
                                             ,"Maybe you misspelled it?"
                                             ,"Otherwise either remove the signature or add an appropriate attribute definition."
                                             ]
                               act  = wfill ["It is not possible to proceed without this declaration."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           MissingUnique nt_ attr_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_ParserError :: Pos ->
                         String ->
                         String ->
                         T_Error 
sem_Error_ParserError pos_ problem_ action_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 69, column 21)
                       _lhsOpp =
                           let mesg = text ("parser expecting " ++ problem_)
                               pat  = text ""
                               help = text ""
                               act  = text action_
                            in ppError (isError _lhsIoptions _me) pos_ mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           ParserError pos_ problem_ action_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_SuperfluousRule :: NontermIdent ->
                             ConstructorIdent ->
                             Identifier ->
                             Identifier ->
                             T_Error 
sem_Error_SuperfluousRule nt_ con_ field_ attr_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 273, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Rule for non-existing", showAttrDef field_ attr_ , "at alternative"
                                             , getName con_ , "of nonterminal",getName nt_, "."
                                             ]
                               pat   =   "SEM" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                               help =  wfill ["There is a rule for" , showAttrDef field_ attr_ , "in the definitions for alternative" , getName con_
                                             ,"of nonterminal" , getName nt_,  ", but this attribute does not exist. Maybe you misspelled it?"
                                             ,"Otherwise either remove the rule or add an appropriate attribute definition."
                                             ]
                               act  = wfill ["The rule has been ignored."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           SuperfluousRule nt_ con_ field_ attr_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_UndefAlt :: NontermIdent ->
                      ConstructorIdent ->
                      T_Error 
sem_Error_UndefAlt nt_ con_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 233, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Constructor", getName con_, "of nonterminal" ,getName nt_, "is  not defined."
                                             ]
                               pat   =   "DATA" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< "...")
                               help =  wfill ["There are rules for alternative", getName con_ , "of nonterminal" ,getName nt_
                                                     ,", but there is no definition for this alternative in the definitions of the"
                                                     ,"nonterminal" , getName nt_, ". Maybe you misspelled it? Otherwise insert a definition."
                                                     ]
                               act  = wfill ["All rules for the unknown alternative have been ignored."]
                           in ppError (isError _lhsIoptions _me) (getPos con_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           UndefAlt nt_ con_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_UndefAttr :: NontermIdent ->
                       ConstructorIdent ->
                       Identifier ->
                       Identifier ->
                       Bool ->
                       T_Error 
sem_Error_UndefAttr nt_ con_ field_ attr_ isOut_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 316, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Undefined"
                                             , if isOut_
                                               then showAttrDef field_ attr_
                                               else showAttrUse field_ attr_
                                             , "at constructor"
                                             , getName con_ , "of nonterminal",getName nt_, "."
                                             ]
                               pat   = "SEM" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< "<field>.<attr> = "
                                                       >#< "..." >#< ppAttrUse field_ attr_ >#< "...")
                               help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                             ,getName  nt_ , "contains an attribute that is not defined"
                                             ,"Maybe you misspelled it?"
                                             ,"Otherwise either remove the rule or add an appropriate attribute definition."
                                             ]
                               act  = wfill ["The generated program will not run."]
                           in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           UndefAttr nt_ con_ field_ attr_ isOut_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_UndefChild :: NontermIdent ->
                        ConstructorIdent ->
                        Identifier ->
                        T_Error 
sem_Error_UndefChild nt_ con_ name_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 245, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Constructor", getName con_, "of nonterminal" ,getName nt_
                                             , "does not have a nontrivial field named", getName name_ , "."
                                             ]
                               pat   =   "SEM" >#< nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< ppAttr name_ (identifier "<attr>") >#< "= ...")
                               help =  wfill ["There are rules that define or use attributes of field" , getName name_
                                                     ,"in alternative" , getName con_ , "of nonterminal" , getName nt_
                                                     ,", but there is no field with AG-type in the definition of the alternative."
                                                     ,"Maybe you misspelled it? Otherwise insert the field into the definition,"
                                                     ,"or change its type from an HS-type to an AG-type."
                                                     ]
                               act  = wfill ["All rules for the unknown field have been ignored."]
                           in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           UndefChild nt_ con_ name_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_UndefLocal :: NontermIdent ->
                        ConstructorIdent ->
                        Identifier ->
                        T_Error 
sem_Error_UndefLocal nt_ con_ var_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 287, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Undefined local variable or field",getName var_, "at constructor"
                                             , getName con_ , "of nonterminal",getName nt_, "."
                                             ]
                               pat   = "SEM" >#< getName nt_
                                     >-< indent 2 ("|" >#< getName con_ >#< "<field>.<attr> = "
                                                       >#< "..." >#< "@" >|< getName var_ >#< "..." )
                               help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                             , getName nt_ , "contains a local variable or field name that is not defined. "
                                             ,"Maybe you misspelled it?"
                                             ,"Otherwise either remove the rule or add an appropriate definition."
                                             ]
                               act  = wfill ["The generated program will not run."]
                           in ppError (isError _lhsIoptions _me) (getPos var_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           UndefLocal nt_ con_ var_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
sem_Error_UndefNont :: NontermIdent ->
                       T_Error 
sem_Error_UndefNont nt_  =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "PrintErrorMessages.ag"(line 223, column 21)
                       _lhsOpp =
                           let mesg  = wfill ["Nonterminal", getName nt_, "is not defined."
                                             ]
                               pat   = "DATA" >#< getName nt_ >#< "..."
                               help =  wfill ["There are attributes and/or rules for nonterminal" , getName nt_  ,", but there is no definition"
                                                     , "for" ,getName  nt_, ". Maybe you misspelled it? Otherwise insert a definition."
                                                     ]
                               act  = wfill ["Everything regarding the unknown nonterminal has been ignored."]
                           in ppError (isError _lhsIoptions _me) (getPos nt_) mesg pat help act _lhsIverbose
                       -- self rule
                       _me =
                           UndefNont nt_
                       -- self rule
                       _lhsOme =
                           _me
                   in  ( _lhsOme,_lhsOpp))) )
-- Errors ------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : Error 
         child tl             : Errors 
         visit 0:
            local verbose     : _
      alternative Nil:
         visit 0:
            local verbose     : _
-}
-- cata
sem_Errors :: Errors  ->
              T_Errors 
sem_Errors list  =
    (Prelude.foldr sem_Errors_Cons sem_Errors_Nil (Prelude.map sem_Error list) )
-- semantic domain
newtype T_Errors  = T_Errors (Options ->
                              ( PP_Doc))
data Inh_Errors  = Inh_Errors {options_Inh_Errors :: Options}
data Syn_Errors  = Syn_Errors {pp_Syn_Errors :: PP_Doc}
wrap_Errors :: T_Errors  ->
               Inh_Errors  ->
               Syn_Errors 
wrap_Errors (T_Errors sem ) (Inh_Errors _lhsIoptions )  =
    (let ( _lhsOpp) =
             (sem _lhsIoptions )
     in  (Syn_Errors _lhsOpp ))
sem_Errors_Cons :: T_Error  ->
                   T_Errors  ->
                   T_Errors 
sem_Errors_Cons (T_Error hd_ ) (T_Errors tl_ )  =
    (T_Errors (\ _lhsIoptions ->
                   (let _lhsOpp :: PP_Doc
                        _hdOoptions :: Options
                        _hdOverbose :: Bool
                        _tlOoptions :: Options
                        _hdIme :: Error
                        _hdIpp :: PP_Doc
                        _tlIpp :: PP_Doc
                        -- "PrintErrorMessages.ag"(line 64, column 8)
                        _verbose =
                            verbose _lhsIoptions
                        -- use rule "PrintErrorMessages.ag"(line 60, column 49)
                        _lhsOpp =
                            _hdIpp >-< _tlIpp
                        -- copy rule (down)
                        _hdOoptions =
                            _lhsIoptions
                        -- copy rule (from local)
                        _hdOverbose =
                            _verbose
                        -- copy rule (down)
                        _tlOoptions =
                            _lhsIoptions
                        ( _hdIme,_hdIpp) =
                            (hd_ _hdOoptions _hdOverbose )
                        ( _tlIpp) =
                            (tl_ _tlOoptions )
                    in  ( _lhsOpp))) )
sem_Errors_Nil :: T_Errors 
sem_Errors_Nil  =
    (T_Errors (\ _lhsIoptions ->
                   (let _lhsOpp :: PP_Doc
                        -- "PrintErrorMessages.ag"(line 64, column 8)
                        _verbose =
                            verbose _lhsIoptions
                        -- use rule "PrintErrorMessages.ag"(line 60, column 49)
                        _lhsOpp =
                            text ""
                    in  ( _lhsOpp))) )