

-- UUAGC 0.9.10 (SemHsTokens.ag)
module SemHsTokens where

import qualified Data.Sequence as Seq
import Data.Sequence(Seq,empty,singleton,(><))
import Data.Foldable(toList)
import Pretty

import TokenDef
import HsToken
import ErrorMessages


import CommonTypes
import UU.Scanner.Position(Pos)

isNTname allnts (Just (NT nt _)) = nt `elem` allnts
isNTname allnts _                = False
-- HsToken -----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         fieldnames           : [Identifier]
         nt                   : Identifier
      synthesized attributes:
         errors               : Seq Error
         tok                  : (Pos,String)
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : Seq Identifier
         usedLocals           : [Identifier]
   alternatives:
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local addTrace    : _
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
         visit 0:
            local _tup1       : _
            local errors      : _
            local tok         : _
            local usedLocals  : _
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
-}
-- cata
sem_HsToken :: HsToken  ->
               T_HsToken 
sem_HsToken (AGField _field _attr _pos _rdesc )  =
    (sem_HsToken_AGField _field _attr _pos _rdesc )
sem_HsToken (AGLocal _var _pos _rdesc )  =
    (sem_HsToken_AGLocal _var _pos _rdesc )
sem_HsToken (CharToken _value _pos )  =
    (sem_HsToken_CharToken _value _pos )
sem_HsToken (Err _mesg _pos )  =
    (sem_HsToken_Err _mesg _pos )
sem_HsToken (HsToken _value _pos )  =
    (sem_HsToken_HsToken _value _pos )
sem_HsToken (StrToken _value _pos )  =
    (sem_HsToken_StrToken _value _pos )
-- semantic domain
newtype T_HsToken  = T_HsToken (([(Identifier,Type,Bool)]) ->
                                ([Identifier]) ->
                                ([(Identifier,Identifier)]) ->
                                Identifier ->
                                ([Identifier]) ->
                                Identifier ->
                                ( (Seq Error),((Pos,String)),([(Identifier,Identifier)]),(Seq Identifier),([Identifier])))
data Inh_HsToken  = Inh_HsToken {allfields_Inh_HsToken :: [(Identifier,Type,Bool)],allnts_Inh_HsToken :: [Identifier],attrs_Inh_HsToken :: [(Identifier,Identifier)],con_Inh_HsToken :: Identifier,fieldnames_Inh_HsToken :: [Identifier],nt_Inh_HsToken :: Identifier}
data Syn_HsToken  = Syn_HsToken {errors_Syn_HsToken :: Seq Error,tok_Syn_HsToken :: (Pos,String),usedAttrs_Syn_HsToken :: [(Identifier,Identifier)],usedFields_Syn_HsToken :: Seq Identifier,usedLocals_Syn_HsToken :: [Identifier]}
wrap_HsToken :: T_HsToken  ->
                Inh_HsToken  ->
                Syn_HsToken 
wrap_HsToken (T_HsToken sem ) (Inh_HsToken _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt )  =
    (let ( _lhsOerrors,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) =
             (sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt )
     in  (Syn_HsToken _lhsOerrors _lhsOtok _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals ))
sem_HsToken_AGField :: Identifier ->
                       Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGField field_ attr_ pos_ rdesc_  =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOtok :: ((Pos,String))
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         -- "SemHsTokens.ag"(line 74, column 15)
                         _lhsOerrors =
                             if (field_,attr_) `elem` _lhsIattrs
                                   then Seq.empty
                                   else if not(field_ `elem` (_LHS : _LOC: _lhsIfieldnames))
                                           then Seq.singleton (UndefChild _lhsInt _lhsIcon field_)
                                           else Seq.singleton (UndefAttr _lhsInt _lhsIcon field_ attr_ False)
                         -- "SemHsTokens.ag"(line 88, column 13)
                         _lhsOusedAttrs =
                             [(field_,attr_)]
                         -- "SemHsTokens.ag"(line 115, column 8)
                         _addTrace =
                             case rdesc_ of
                               Just d  -> \x -> "(trace " ++ show (d ++ " -> " ++ show field_ ++ "." ++ show attr_) ++ " (" ++ x ++ "))"
                               Nothing -> id
                         -- "SemHsTokens.ag"(line 118, column 8)
                         _lhsOtok =
                             (pos_, _addTrace     $ attrname True field_ attr_)
                         -- use rule "SemHsTokens.ag"(line 93, column 40)
                         _lhsOusedFields =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 84, column 40)
                         _lhsOusedLocals =
                             []
                     in  ( _lhsOerrors,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
sem_HsToken_AGLocal :: Identifier ->
                       Pos ->
                       (Maybe String) ->
                       T_HsToken 
sem_HsToken_AGLocal var_ pos_ rdesc_  =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOusedFields :: (Seq Identifier)
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedLocals :: ([Identifier])
                         _lhsOtok :: ((Pos,String))
                         -- "SemHsTokens.ag"(line 64, column 19)
                         __tup1 =
                             if var_ `elem` _lhsIfieldnames
                             then if  isNTname _lhsIallnts (lookup var_ (map (\(n,t,_) -> (n,t)) _lhsIallfields))
                                  then (Seq.singleton(ChildAsLocal _lhsInt _lhsIcon var_), (pos_,fieldname var_), []    )
                                  else (Seq.empty, (pos_,fieldname var_), []    )
                             else if (_LOC,var_) `elem` _lhsIattrs
                             then (Seq.empty                                   , (pos_,locname   var_), [var_])
                             else (Seq.singleton(UndefLocal _lhsInt _lhsIcon var_), (pos_,locname   var_), []    )
                         -- "SemHsTokens.ag"(line 64, column 19)
                         (_errors,_,_) =
                             __tup1
                         -- "SemHsTokens.ag"(line 64, column 19)
                         (_,_tok,_) =
                             __tup1
                         -- "SemHsTokens.ag"(line 64, column 19)
                         (_,_,_usedLocals) =
                             __tup1
                         -- "SemHsTokens.ag"(line 96, column 13)
                         _lhsOusedFields =
                             if var_ `elem` _lhsIfieldnames
                              then Seq.singleton var_
                              else Seq.empty
                         -- use rule "SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             _errors
                         -- use rule "SemHsTokens.ag"(line 85, column 40)
                         _lhsOusedAttrs =
                             []
                         -- use rule "SemHsTokens.ag"(line 84, column 40)
                         _lhsOusedLocals =
                             _usedLocals
                         -- copy rule (from local)
                         _lhsOtok =
                             _tok
                     in  ( _lhsOerrors,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
sem_HsToken_CharToken :: String ->
                         Pos ->
                         T_HsToken 
sem_HsToken_CharToken value_ pos_  =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOtok :: ((Pos,String))
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         -- "SemHsTokens.ag"(line 122, column 16)
                         _lhsOtok =
                             (pos_, if null value_
                                       then ""
                                       else showCharShort (head value_)
                             )
                         -- use rule "SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 85, column 40)
                         _lhsOusedAttrs =
                             []
                         -- use rule "SemHsTokens.ag"(line 93, column 40)
                         _lhsOusedFields =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 84, column 40)
                         _lhsOusedLocals =
                             []
                     in  ( _lhsOerrors,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
sem_HsToken_Err :: String ->
                   Pos ->
                   T_HsToken 
sem_HsToken_Err mesg_ pos_  =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOerrors :: (Seq Error)
                         _lhsOtok :: ((Pos,String))
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         -- "SemHsTokens.ag"(line 50, column 9)
                         _lhsOerrors =
                             let m = text mesg_
                             in Seq.singleton (CustomError False pos_ m)
                         -- "SemHsTokens.ag"(line 128, column 16)
                         _lhsOtok =
                             (pos_, "")
                         -- use rule "SemHsTokens.ag"(line 85, column 40)
                         _lhsOusedAttrs =
                             []
                         -- use rule "SemHsTokens.ag"(line 93, column 40)
                         _lhsOusedFields =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 84, column 40)
                         _lhsOusedLocals =
                             []
                     in  ( _lhsOerrors,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
sem_HsToken_HsToken :: String ->
                       Pos ->
                       T_HsToken 
sem_HsToken_HsToken value_ pos_  =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOtok :: ((Pos,String))
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         -- "SemHsTokens.ag"(line 120, column 14)
                         _lhsOtok =
                             (pos_, value_)
                         -- use rule "SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 85, column 40)
                         _lhsOusedAttrs =
                             []
                         -- use rule "SemHsTokens.ag"(line 93, column 40)
                         _lhsOusedFields =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 84, column 40)
                         _lhsOusedLocals =
                             []
                     in  ( _lhsOerrors,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
sem_HsToken_StrToken :: String ->
                        Pos ->
                        T_HsToken 
sem_HsToken_StrToken value_ pos_  =
    (T_HsToken (\ _lhsIallfields
                  _lhsIallnts
                  _lhsIattrs
                  _lhsIcon
                  _lhsIfieldnames
                  _lhsInt ->
                    (let _lhsOtok :: ((Pos,String))
                         _lhsOerrors :: (Seq Error)
                         _lhsOusedAttrs :: ([(Identifier,Identifier)])
                         _lhsOusedFields :: (Seq Identifier)
                         _lhsOusedLocals :: ([Identifier])
                         -- "SemHsTokens.ag"(line 127, column 16)
                         _lhsOtok =
                             (pos_, showStrShort value_)
                         -- use rule "SemHsTokens.ag"(line 43, column 37)
                         _lhsOerrors =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 85, column 40)
                         _lhsOusedAttrs =
                             []
                         -- use rule "SemHsTokens.ag"(line 93, column 40)
                         _lhsOusedFields =
                             Seq.empty
                         -- use rule "SemHsTokens.ag"(line 84, column 40)
                         _lhsOusedLocals =
                             []
                     in  ( _lhsOerrors,_lhsOtok,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
-- HsTokens ----------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         fieldnames           : [Identifier]
         nt                   : Identifier
      synthesized attributes:
         errors               : Seq Error
         tks                  : [(Pos,String)]
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : Seq Identifier
         usedLocals           : [Identifier]
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
-- cata
sem_HsTokens :: HsTokens  ->
                T_HsTokens 
sem_HsTokens list  =
    (Prelude.foldr sem_HsTokens_Cons sem_HsTokens_Nil (Prelude.map sem_HsToken list) )
-- semantic domain
newtype T_HsTokens  = T_HsTokens (([(Identifier,Type,Bool)]) ->
                                  ([Identifier]) ->
                                  ([(Identifier,Identifier)]) ->
                                  Identifier ->
                                  ([Identifier]) ->
                                  Identifier ->
                                  ( (Seq Error),([(Pos,String)]),([(Identifier,Identifier)]),(Seq Identifier),([Identifier])))
data Inh_HsTokens  = Inh_HsTokens {allfields_Inh_HsTokens :: [(Identifier,Type,Bool)],allnts_Inh_HsTokens :: [Identifier],attrs_Inh_HsTokens :: [(Identifier,Identifier)],con_Inh_HsTokens :: Identifier,fieldnames_Inh_HsTokens :: [Identifier],nt_Inh_HsTokens :: Identifier}
data Syn_HsTokens  = Syn_HsTokens {errors_Syn_HsTokens :: Seq Error,tks_Syn_HsTokens :: [(Pos,String)],usedAttrs_Syn_HsTokens :: [(Identifier,Identifier)],usedFields_Syn_HsTokens :: Seq Identifier,usedLocals_Syn_HsTokens :: [Identifier]}
wrap_HsTokens :: T_HsTokens  ->
                 Inh_HsTokens  ->
                 Syn_HsTokens 
wrap_HsTokens (T_HsTokens sem ) (Inh_HsTokens _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt )  =
    (let ( _lhsOerrors,_lhsOtks,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) =
             (sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsIfieldnames _lhsInt )
     in  (Syn_HsTokens _lhsOerrors _lhsOtks _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals ))
sem_HsTokens_Cons :: T_HsToken  ->
                     T_HsTokens  ->
                     T_HsTokens 
sem_HsTokens_Cons (T_HsToken hd_ ) (T_HsTokens tl_ )  =
    (T_HsTokens (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIfieldnames
                   _lhsInt ->
                     (let _lhsOtks :: ([(Pos,String)])
                          _lhsOerrors :: (Seq Error)
                          _lhsOusedAttrs :: ([(Identifier,Identifier)])
                          _lhsOusedFields :: (Seq Identifier)
                          _lhsOusedLocals :: ([Identifier])
                          _hdOallfields :: ([(Identifier,Type,Bool)])
                          _hdOallnts :: ([Identifier])
                          _hdOattrs :: ([(Identifier,Identifier)])
                          _hdOcon :: Identifier
                          _hdOfieldnames :: ([Identifier])
                          _hdOnt :: Identifier
                          _tlOallfields :: ([(Identifier,Type,Bool)])
                          _tlOallnts :: ([Identifier])
                          _tlOattrs :: ([(Identifier,Identifier)])
                          _tlOcon :: Identifier
                          _tlOfieldnames :: ([Identifier])
                          _tlOnt :: Identifier
                          _hdIerrors :: (Seq Error)
                          _hdItok :: ((Pos,String))
                          _hdIusedAttrs :: ([(Identifier,Identifier)])
                          _hdIusedFields :: (Seq Identifier)
                          _hdIusedLocals :: ([Identifier])
                          _tlIerrors :: (Seq Error)
                          _tlItks :: ([(Pos,String)])
                          _tlIusedAttrs :: ([(Identifier,Identifier)])
                          _tlIusedFields :: (Seq Identifier)
                          _tlIusedLocals :: ([Identifier])
                          -- "SemHsTokens.ag"(line 110, column 10)
                          _lhsOtks =
                              _hdItok : _tlItks
                          -- use rule "SemHsTokens.ag"(line 43, column 37)
                          _lhsOerrors =
                              _hdIerrors Seq.>< _tlIerrors
                          -- use rule "SemHsTokens.ag"(line 85, column 40)
                          _lhsOusedAttrs =
                              _hdIusedAttrs ++ _tlIusedAttrs
                          -- use rule "SemHsTokens.ag"(line 93, column 40)
                          _lhsOusedFields =
                              _hdIusedFields Seq.>< _tlIusedFields
                          -- use rule "SemHsTokens.ag"(line 84, column 40)
                          _lhsOusedLocals =
                              _hdIusedLocals ++ _tlIusedLocals
                          -- copy rule (down)
                          _hdOallfields =
                              _lhsIallfields
                          -- copy rule (down)
                          _hdOallnts =
                              _lhsIallnts
                          -- copy rule (down)
                          _hdOattrs =
                              _lhsIattrs
                          -- copy rule (down)
                          _hdOcon =
                              _lhsIcon
                          -- copy rule (down)
                          _hdOfieldnames =
                              _lhsIfieldnames
                          -- copy rule (down)
                          _hdOnt =
                              _lhsInt
                          -- copy rule (down)
                          _tlOallfields =
                              _lhsIallfields
                          -- copy rule (down)
                          _tlOallnts =
                              _lhsIallnts
                          -- copy rule (down)
                          _tlOattrs =
                              _lhsIattrs
                          -- copy rule (down)
                          _tlOcon =
                              _lhsIcon
                          -- copy rule (down)
                          _tlOfieldnames =
                              _lhsIfieldnames
                          -- copy rule (down)
                          _tlOnt =
                              _lhsInt
                          ( _hdIerrors,_hdItok,_hdIusedAttrs,_hdIusedFields,_hdIusedLocals) =
                              (hd_ _hdOallfields _hdOallnts _hdOattrs _hdOcon _hdOfieldnames _hdOnt )
                          ( _tlIerrors,_tlItks,_tlIusedAttrs,_tlIusedFields,_tlIusedLocals) =
                              (tl_ _tlOallfields _tlOallnts _tlOattrs _tlOcon _tlOfieldnames _tlOnt )
                      in  ( _lhsOerrors,_lhsOtks,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
sem_HsTokens_Nil :: T_HsTokens 
sem_HsTokens_Nil  =
    (T_HsTokens (\ _lhsIallfields
                   _lhsIallnts
                   _lhsIattrs
                   _lhsIcon
                   _lhsIfieldnames
                   _lhsInt ->
                     (let _lhsOtks :: ([(Pos,String)])
                          _lhsOerrors :: (Seq Error)
                          _lhsOusedAttrs :: ([(Identifier,Identifier)])
                          _lhsOusedFields :: (Seq Identifier)
                          _lhsOusedLocals :: ([Identifier])
                          -- "SemHsTokens.ag"(line 111, column 10)
                          _lhsOtks =
                              []
                          -- use rule "SemHsTokens.ag"(line 43, column 37)
                          _lhsOerrors =
                              Seq.empty
                          -- use rule "SemHsTokens.ag"(line 85, column 40)
                          _lhsOusedAttrs =
                              []
                          -- use rule "SemHsTokens.ag"(line 93, column 40)
                          _lhsOusedFields =
                              Seq.empty
                          -- use rule "SemHsTokens.ag"(line 84, column 40)
                          _lhsOusedLocals =
                              []
                      in  ( _lhsOerrors,_lhsOtks,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )
-- HsTokensRoot ------------------------------------------------
{-
   visit 0:
      inherited attributes:
         allfields            : [(Identifier,Type,Bool)]
         allnts               : [Identifier]
         attrs                : [(Identifier,Identifier)]
         con                  : Identifier
         nt                   : Identifier
      synthesized attributes:
         errors               : Seq Error
         textLines            : [String]
         usedAttrs            : [(Identifier,Identifier)]
         usedFields           : [Identifier]
         usedLocals           : [Identifier]
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
-- cata
sem_HsTokensRoot :: HsTokensRoot  ->
                    T_HsTokensRoot 
sem_HsTokensRoot (HsTokensRoot _tokens )  =
    (sem_HsTokensRoot_HsTokensRoot (sem_HsTokens _tokens ) )
-- semantic domain
newtype T_HsTokensRoot  = T_HsTokensRoot (([(Identifier,Type,Bool)]) ->
                                          ([Identifier]) ->
                                          ([(Identifier,Identifier)]) ->
                                          Identifier ->
                                          Identifier ->
                                          ( (Seq Error),([String]),([(Identifier,Identifier)]),([Identifier]),([Identifier])))
data Inh_HsTokensRoot  = Inh_HsTokensRoot {allfields_Inh_HsTokensRoot :: [(Identifier,Type,Bool)],allnts_Inh_HsTokensRoot :: [Identifier],attrs_Inh_HsTokensRoot :: [(Identifier,Identifier)],con_Inh_HsTokensRoot :: Identifier,nt_Inh_HsTokensRoot :: Identifier}
data Syn_HsTokensRoot  = Syn_HsTokensRoot {errors_Syn_HsTokensRoot :: Seq Error,textLines_Syn_HsTokensRoot :: [String],usedAttrs_Syn_HsTokensRoot :: [(Identifier,Identifier)],usedFields_Syn_HsTokensRoot :: [Identifier],usedLocals_Syn_HsTokensRoot :: [Identifier]}
wrap_HsTokensRoot :: T_HsTokensRoot  ->
                     Inh_HsTokensRoot  ->
                     Syn_HsTokensRoot 
wrap_HsTokensRoot (T_HsTokensRoot sem ) (Inh_HsTokensRoot _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt )  =
    (let ( _lhsOerrors,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals) =
             (sem _lhsIallfields _lhsIallnts _lhsIattrs _lhsIcon _lhsInt )
     in  (Syn_HsTokensRoot _lhsOerrors _lhsOtextLines _lhsOusedAttrs _lhsOusedFields _lhsOusedLocals ))
sem_HsTokensRoot_HsTokensRoot :: T_HsTokens  ->
                                 T_HsTokensRoot 
sem_HsTokensRoot_HsTokensRoot (T_HsTokens tokens_ )  =
    (T_HsTokensRoot (\ _lhsIallfields
                       _lhsIallnts
                       _lhsIattrs
                       _lhsIcon
                       _lhsInt ->
                         (let _tokensOfieldnames :: ([Identifier])
                              _lhsOusedFields :: ([Identifier])
                              _lhsOtextLines :: ([String])
                              _lhsOerrors :: (Seq Error)
                              _lhsOusedAttrs :: ([(Identifier,Identifier)])
                              _lhsOusedLocals :: ([Identifier])
                              _tokensOallfields :: ([(Identifier,Type,Bool)])
                              _tokensOallnts :: ([Identifier])
                              _tokensOattrs :: ([(Identifier,Identifier)])
                              _tokensOcon :: Identifier
                              _tokensOnt :: Identifier
                              _tokensIerrors :: (Seq Error)
                              _tokensItks :: ([(Pos,String)])
                              _tokensIusedAttrs :: ([(Identifier,Identifier)])
                              _tokensIusedFields :: (Seq Identifier)
                              _tokensIusedLocals :: ([Identifier])
                              -- "SemHsTokens.ag"(line 38, column 18)
                              _tokensOfieldnames =
                                  map (\(n,_,_) -> n) _lhsIallfields
                              -- "SemHsTokens.ag"(line 100, column 18)
                              _lhsOusedFields =
                                  toList _tokensIusedFields
                              -- "SemHsTokens.ag"(line 107, column 18)
                              _lhsOtextLines =
                                  showTokens _tokensItks
                              -- use rule "SemHsTokens.ag"(line 18, column 18)
                              _lhsOerrors =
                                  _tokensIerrors
                              -- copy rule (up)
                              _lhsOusedAttrs =
                                  _tokensIusedAttrs
                              -- copy rule (up)
                              _lhsOusedLocals =
                                  _tokensIusedLocals
                              -- copy rule (down)
                              _tokensOallfields =
                                  _lhsIallfields
                              -- copy rule (down)
                              _tokensOallnts =
                                  _lhsIallnts
                              -- copy rule (down)
                              _tokensOattrs =
                                  _lhsIattrs
                              -- copy rule (down)
                              _tokensOcon =
                                  _lhsIcon
                              -- copy rule (down)
                              _tokensOnt =
                                  _lhsInt
                              ( _tokensIerrors,_tokensItks,_tokensIusedAttrs,_tokensIusedFields,_tokensIusedLocals) =
                                  (tokens_ _tokensOallfields _tokensOallnts _tokensOattrs _tokensOcon _tokensOfieldnames _tokensOnt )
                          in  ( _lhsOerrors,_lhsOtextLines,_lhsOusedAttrs,_lhsOusedFields,_lhsOusedLocals))) )