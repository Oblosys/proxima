module ProxParser_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Presentation.PresLayerTypes
import Presentation.PresLayerUtils

import Evaluation.DocumentEdit
import DocumentEdit_Generated
import DocUtils_Generated
import Evaluation.DocTypes
import DocTypes_Generated
import Presentation.PresentationParsing
import Data.Maybe
                                   
----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Construct instance                                                   --
--------------------------------------------------------------------------

instance Construct Document Node ClipDoc UserToken where
  construct NoNode = error $ "ProxParser_Generated.construct not defined on NoNode"
  construct (Node_RootEnr _ _) = construct_RootEnr
  construct (Node_HoleEnrichedDoc _ _) = construct_HoleEnrichedDoc
  construct (Node_ParseErrEnrichedDoc _ _) = construct_ParseErrEnrichedDoc
  construct (Node_RootDoc _ _) = construct_RootDoc
  construct (Node_HoleDocument _ _) = construct_HoleDocument
  construct (Node_ParseErrDocument _ _) = construct_ParseErrDocument
  construct (Node_Form _ _) = construct_Form
  construct (Node_HoleForm _ _) = construct_HoleForm
  construct (Node_ParseErrForm _ _) = construct_ParseErrForm
  construct (Node_Expense _ _) = construct_Expense
  construct (Node_HoleExpense _ _) = construct_HoleExpense
  construct (Node_ParseErrExpense _ _) = construct_ParseErrExpense
  construct (Node_Currency _ _) = construct_Currency
  construct (Node_HoleCurrency _ _) = construct_HoleCurrency
  construct (Node_ParseErrCurrency _ _) = construct_ParseErrCurrency
  construct (Node_List_Expense _ _) = construct_List_Expense
  construct (Node_HoleList_Expense _ _) = construct_HoleList_Expense
  construct (Node_ParseErrList_Expense _ _) = construct_ParseErrList_Expense
  construct (Node_List_Currency _ _) = construct_List_Currency
  construct (Node_HoleList_Currency _ _) = construct_HoleList_Currency
  construct (Node_ParseErrList_Currency _ _) = construct_ParseErrList_Currency
construct_RootEnr tk ~[mClip0] = Clip_EnrichedDoc $ reuseRootEnr [tk]  (retrieveArg "RootEnr" "form::Form" mClip0)
construct_HoleEnrichedDoc tk ~[] = Clip_EnrichedDoc $ hole
construct_ParseErrEnrichedDoc (StructuralTk _ _ pres _ _) ~[] = Clip_EnrichedDoc $ parseErr (StructuralParseErr pres)
construct_RootDoc tk ~[mClip0] = Clip_Document $ reuseRootDoc [tk]  (retrieveArg "RootDoc" "form::Form" mClip0)
construct_HoleDocument tk ~[] = Clip_Document $ hole
construct_ParseErrDocument (StructuralTk _ _ pres _ _) ~[] = Clip_Document $ parseErr (StructuralParseErr pres)
construct_Form tk ~[mClip0,mClip1,mClip2,mClip3] = Clip_Form $ reuseForm [tk]  (retrieveArg "Form" "name::String" mClip0) (retrieveArg "Form" "faculty::String" mClip1) (retrieveArg "Form" "expenses::List_Expense" mClip2) (retrieveArg "Form" "currencies::List_Currency" mClip3)
construct_HoleForm tk ~[] = Clip_Form $ hole
construct_ParseErrForm (StructuralTk _ _ pres _ _) ~[] = Clip_Form $ parseErr (StructuralParseErr pres)
construct_Expense tk ~[mClip0,mClip1,mClip2] = Clip_Expense $ reuseExpense [tk]  (retrieveArg "Expense" "description::String" mClip0) (retrieveArg "Expense" "amount::Float" mClip1) (retrieveArg "Expense" "currencyIx::Int" mClip2)
construct_HoleExpense tk ~[] = Clip_Expense $ hole
construct_ParseErrExpense (StructuralTk _ _ pres _ _) ~[] = Clip_Expense $ parseErr (StructuralParseErr pres)
construct_Currency tk ~[mClip0,mClip1] = Clip_Currency $ reuseCurrency [tk]  (retrieveArg "Currency" "name::String" mClip0) (retrieveArg "Currency" "euroRate::Float" mClip1)
construct_HoleCurrency tk ~[] = Clip_Currency $ hole
construct_ParseErrCurrency (StructuralTk _ _ pres _ _) ~[] = Clip_Currency $ parseErr (StructuralParseErr pres)
construct_List_Expense tk mClips = genericConstruct_List "Expense" toList_Expense mClips
construct_HoleList_Expense tk ~[] = Clip_List_Expense $ hole
construct_ParseErrList_Expense (StructuralTk _ _ pres _ _) ~[] = Clip_List_Expense $ parseErr (StructuralParseErr pres)
construct_List_Currency tk mClips = genericConstruct_List "Currency" toList_Currency mClips
construct_HoleList_Currency tk ~[] = Clip_List_Currency $ hole
construct_ParseErrList_Currency (StructuralTk _ _ pres _ _) ~[] = Clip_List_Currency $ parseErr (StructuralParseErr pres)



--------------------------------------------------------------------------
-- reuse functions                                                      --
--------------------------------------------------------------------------

reuseRootEnr :: [Token doc Node clip token] -> Maybe Form -> EnrichedDoc
reuseRootEnr nodes ma0
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0) -> genericReuse1 RootEnr a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRootEnr"

reuseRootDoc :: [Token doc Node clip token] -> Maybe Form -> Document
reuseRootDoc nodes ma0
  = case extractFromTokens extractRootDoc defaultRootDoc nodes of
           (RootDoc a0) -> genericReuse1 RootDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRootDoc"

reuseForm :: [Token doc Node clip token] -> Maybe String -> Maybe String -> Maybe List_Expense -> Maybe List_Currency -> Form
reuseForm nodes ma0 ma1 ma2 ma3
  = case extractFromTokens extractForm defaultForm nodes of
           (Form a0 a1 a2 a3) -> genericReuse4 Form a0 a1 a2 a3 ma0 ma1 ma2 ma3
           _ -> error "Internal error:ProxParser_Generated.reuseForm"

reuseExpense :: [Token doc Node clip token] -> Maybe String -> Maybe Float -> Maybe Int -> Expense
reuseExpense nodes ma0 ma1 ma2
  = case extractFromTokens extractExpense defaultExpense nodes of
           (Expense a0 a1 a2) -> genericReuse3 Expense a0 a1 a2 ma0 ma1 ma2
           _ -> error "Internal error:ProxParser_Generated.reuseExpense"

reuseCurrency :: [Token doc Node clip token] -> Maybe String -> Maybe Float -> Currency
reuseCurrency nodes ma0 ma1
  = case extractFromTokens extractCurrency defaultCurrency nodes of
           (Currency a0 a1) -> genericReuse2 Currency a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseCurrency"

reuseList_Expense :: [Token doc Node clip token] -> Maybe ConsList_Expense -> List_Expense
reuseList_Expense nodes ma0
  = case extractFromTokens extractList_Expense defaultList_Expense nodes of
           (List_Expense a0) -> genericReuse1 List_Expense a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Expense"

reuseList_Currency :: [Token doc Node clip token] -> Maybe ConsList_Currency -> List_Currency
reuseList_Currency nodes ma0
  = case extractFromTokens extractList_Currency defaultList_Currency nodes of
           (List_Currency a0) -> genericReuse1 List_Currency a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseList_Currency"




--------------------------------------------------------------------------
-- extract functions                                                    --
--------------------------------------------------------------------------

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (Node_RootEnr x@(RootEnr _) _)) = Just x
extractRootEnr _ = Nothing

extractRootDoc :: Maybe Node -> Maybe Document
extractRootDoc (Just (Node_RootDoc x@(RootDoc _) _)) = Just x
extractRootDoc _ = Nothing

extractForm :: Maybe Node -> Maybe Form
extractForm (Just (Node_Form x@(Form _ _ _ _) _)) = Just x
extractForm _ = Nothing

extractExpense :: Maybe Node -> Maybe Expense
extractExpense (Just (Node_Expense x@(Expense _ _ _) _)) = Just x
extractExpense _ = Nothing

extractCurrency :: Maybe Node -> Maybe Currency
extractCurrency (Just (Node_Currency x@(Currency _ _) _)) = Just x
extractCurrency _ = Nothing

extractList_Expense :: Maybe Node -> Maybe List_Expense
extractList_Expense (Just (Node_List_Expense x@(List_Expense _) _)) = Just x
extractList_Expense _ = Nothing

extractList_Currency :: Maybe Node -> Maybe List_Currency
extractList_Currency (Just (Node_List_Currency x@(List_Currency _) _)) = Just x
extractList_Currency _ = Nothing




--------------------------------------------------------------------------
-- default functions                                                    --
--------------------------------------------------------------------------

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr hole

defaultRootDoc :: Document
defaultRootDoc = RootDoc hole

defaultForm :: Form
defaultForm = Form hole hole hole hole

defaultExpense :: Expense
defaultExpense = Expense hole hole hole

defaultCurrency :: Currency
defaultCurrency = Currency hole hole

defaultList_Expense :: List_Expense
defaultList_Expense = List_Expense Nil_Expense

defaultList_Currency :: List_Currency
defaultList_Currency = List_Currency Nil_Currency




--------------------------------------------------------------------------
-- extractFromTokens                                                    --
--------------------------------------------------------------------------

-- return result of the first extraction application in the list that is not Nothing
extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc Node clip token] -> a
extractFromTokens extr def []     = def
extractFromTokens extr def (t:ts) = maybe (extractFromTokens extr def ts) id (extr (tokenNode t))



--------------------------------------------------------------------------
-- genericReuse functions                                               --
--------------------------------------------------------------------------

genericReuse0 :: (r) ->
                 
                 r
genericReuse0 f =
  f

genericReuse1 :: (a0 -> r) ->
                 a0 -> 
                 Maybe a0 -> r
genericReuse1 f a0 ma0 =
  f (maybe a0 id ma0)

genericReuse2 :: (a0 -> a1 -> r) ->
                 a0 -> a1 -> 
                 Maybe a0 -> Maybe a1 -> r
genericReuse2 f a0 a1 ma0 ma1 =
  f (maybe a0 id ma0) (maybe a1 id ma1)

genericReuse3 :: (a0 -> a1 -> a2 -> r) ->
                 a0 -> a1 -> a2 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> r
genericReuse3 f a0 a1 a2 ma0 ma1 ma2 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2)

genericReuse4 :: (a0 -> a1 -> a2 -> a3 -> r) ->
                 a0 -> a1 -> a2 -> a3 -> 
                 Maybe a0 -> Maybe a1 -> Maybe a2 -> Maybe a3 -> r
genericReuse4 f a0 a1 a2 a3 ma0 ma1 ma2 ma3 =
  f (maybe a0 id ma0) (maybe a1 id ma1) (maybe a2 id ma2) (maybe a3 id ma3)



