module ProxParser_Generated where

import Common.CommonTypes hiding (Dirty (..), defaultTextColor)
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
-- ProxParser type synonym                                              --
--------------------------------------------------------------------------

type ProxParser a = ListParser Document EnrichedDoc Node ClipDoc UserToken a



--------------------------------------------------------------------------
-- Construct instance                                                   --
--------------------------------------------------------------------------

instance Construct Document EnrichedDoc Node ClipDoc UserToken where
  construct NoNode = error $ "ProxParser_Generated.construct not defined on NoNode"
  construct (Node_RootEnr _ _) = construct_RootEnr
  construct (Node_HoleEnrichedDoc _ _) = construct_HoleEnrichedDoc
  construct (Node_ParseErrEnrichedDoc _ _) = construct_ParseErrEnrichedDoc
  construct (Node_RootDoc _ _) = construct_RootDoc
  construct (Node_HoleDocument _ _) = construct_HoleDocument
  construct (Node_ParseErrDocument _ _) = construct_ParseErrDocument
  construct (Node_SudokuDoc _ _) = construct_SudokuDoc
  construct (Node_HoleChoiceDoc _ _) = construct_HoleChoiceDoc
  construct (Node_ParseErrChoiceDoc _ _) = construct_ParseErrChoiceDoc
  construct (Node_Sudoku _ _) = construct_Sudoku
  construct (Node_HoleSudoku _ _) = construct_HoleSudoku
  construct (Node_ParseErrSudoku _ _) = construct_ParseErrSudoku
  construct (Node_Float_ _ _) = construct_Float_
  construct (Node_HoleFloat_ _ _) = construct_HoleFloat_
  construct (Node_ParseErrFloat_ _ _) = construct_ParseErrFloat_
  construct (Node_Fload_ _ _) = construct_Fload_
  construct (Node_HoleFload_ _ _) = construct_HoleFload_
  construct (Node_ParseErrFload_ _ _) = construct_ParseErrFload_
construct_RootEnr tk ~[mClip0] = Clip_EnrichedDoc $ reuseRootEnr [tk]  (retrieveArg "RootEnr" "choiceDoc::ChoiceDoc" mClip0)
construct_HoleEnrichedDoc tk ~[] = Clip_EnrichedDoc $ hole
construct_ParseErrEnrichedDoc (StructuralTk _ _ pres _ _) ~[] = Clip_EnrichedDoc $ parseErr (StructuralParseErr pres)
construct_RootDoc tk ~[mClip0] = Clip_Document $ reuseRootDoc [tk]  (retrieveArg "RootDoc" "choiceDoc::ChoiceDoc" mClip0)
construct_HoleDocument tk ~[] = Clip_Document $ hole
construct_ParseErrDocument (StructuralTk _ _ pres _ _) ~[] = Clip_Document $ parseErr (StructuralParseErr pres)
construct_SudokuDoc tk ~[mClip0] = Clip_ChoiceDoc $ reuseSudokuDoc [tk]  (retrieveArg "SudokuDoc" "sudoku::Sudoku" mClip0)
construct_HoleChoiceDoc tk ~[] = Clip_ChoiceDoc $ hole
construct_ParseErrChoiceDoc (StructuralTk _ _ pres _ _) ~[] = Clip_ChoiceDoc $ parseErr (StructuralParseErr pres)
construct_Sudoku tk ~[mClip0,mClip1] = Clip_Sudoku $ reuseSudoku [tk]  (retrieveArg "Sudoku" "f::Float_" mClip0) (retrieveArg "Sudoku" "i::Fload_" mClip1)
construct_HoleSudoku tk ~[] = Clip_Sudoku $ hole
construct_ParseErrSudoku (StructuralTk _ _ pres _ _) ~[] = Clip_Sudoku $ parseErr (StructuralParseErr pres)
construct_Float_ tk ~[mClip0] = Clip_Float_ $ reuseFloat_ [tk]  (retrieveArg "Float_" "value::Float" mClip0)
construct_HoleFloat_ tk ~[] = Clip_Float_ $ hole
construct_ParseErrFloat_ (StructuralTk _ _ pres _ _) ~[] = Clip_Float_ $ parseErr (StructuralParseErr pres)
construct_Fload_ tk ~[mClip0] = Clip_Fload_ $ reuseFload_ [tk]  (retrieveArg "Fload_" "value::Float" mClip0)
construct_HoleFload_ tk ~[] = Clip_Fload_ $ hole
construct_ParseErrFload_ (StructuralTk _ _ pres _ _) ~[] = Clip_Fload_ $ parseErr (StructuralParseErr pres)



--------------------------------------------------------------------------
-- reuse functions                                                      --
--------------------------------------------------------------------------

reuseRootEnr :: [Token doc enr Node clip token] -> Maybe ChoiceDoc -> EnrichedDoc
reuseRootEnr nodes ma0
  = case extractFromTokens extractRootEnr defaultRootEnr nodes of
           (RootEnr a0) -> genericReuse1 RootEnr a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRootEnr"

reuseRootDoc :: [Token doc enr Node clip token] -> Maybe ChoiceDoc -> Document
reuseRootDoc nodes ma0
  = case extractFromTokens extractRootDoc defaultRootDoc nodes of
           (RootDoc a0) -> genericReuse1 RootDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseRootDoc"

reuseSudokuDoc :: [Token doc enr Node clip token] -> Maybe Sudoku -> ChoiceDoc
reuseSudokuDoc nodes ma0
  = case extractFromTokens extractSudokuDoc defaultSudokuDoc nodes of
           (SudokuDoc a0) -> genericReuse1 SudokuDoc a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseSudokuDoc"

reuseSudoku :: [Token doc enr Node clip token] -> Maybe Float_ -> Maybe Fload_ -> Sudoku
reuseSudoku nodes ma0 ma1
  = case extractFromTokens extractSudoku defaultSudoku nodes of
           (Sudoku a0 a1) -> genericReuse2 Sudoku a0 a1 ma0 ma1
           _ -> error "Internal error:ProxParser_Generated.reuseSudoku"

reuseFloat_ :: [Token doc enr Node clip token] -> Maybe Float -> Float_
reuseFloat_ nodes ma0
  = case extractFromTokens extractFloat_ defaultFloat_ nodes of
           (Float_ a0) -> genericReuse1 Float_ a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseFloat_"

reuseFload_ :: [Token doc enr Node clip token] -> Maybe Float -> Fload_
reuseFload_ nodes ma0
  = case extractFromTokens extractFload_ defaultFload_ nodes of
           (Fload_ a0) -> genericReuse1 Fload_ a0 ma0
           _ -> error "Internal error:ProxParser_Generated.reuseFload_"




--------------------------------------------------------------------------
-- extract functions                                                    --
--------------------------------------------------------------------------

extractRootEnr :: Maybe Node -> Maybe EnrichedDoc
extractRootEnr (Just (Node_RootEnr x@(RootEnr _) _)) = Just x
extractRootEnr _ = Nothing

extractRootDoc :: Maybe Node -> Maybe Document
extractRootDoc (Just (Node_RootDoc x@(RootDoc _) _)) = Just x
extractRootDoc _ = Nothing

extractSudokuDoc :: Maybe Node -> Maybe ChoiceDoc
extractSudokuDoc (Just (Node_SudokuDoc x@(SudokuDoc _) _)) = Just x
extractSudokuDoc _ = Nothing

extractSudoku :: Maybe Node -> Maybe Sudoku
extractSudoku (Just (Node_Sudoku x@(Sudoku _ _) _)) = Just x
extractSudoku _ = Nothing

extractFloat_ :: Maybe Node -> Maybe Float_
extractFloat_ (Just (Node_Float_ x@(Float_ _) _)) = Just x
extractFloat_ _ = Nothing

extractFload_ :: Maybe Node -> Maybe Fload_
extractFload_ (Just (Node_Fload_ x@(Fload_ _) _)) = Just x
extractFload_ _ = Nothing




--------------------------------------------------------------------------
-- default functions                                                    --
--------------------------------------------------------------------------

defaultRootEnr :: EnrichedDoc
defaultRootEnr = RootEnr hole

defaultRootDoc :: Document
defaultRootDoc = RootDoc hole

defaultSudokuDoc :: ChoiceDoc
defaultSudokuDoc = SudokuDoc hole

defaultSudoku :: Sudoku
defaultSudoku = Sudoku hole hole

defaultFloat_ :: Float_
defaultFloat_ = Float_ hole

defaultFload_ :: Fload_
defaultFload_ = Fload_ hole




--------------------------------------------------------------------------
-- extractFromTokens                                                    --
--------------------------------------------------------------------------

-- return result of the first extraction application in the list that is not Nothing
extractFromTokens :: (Maybe Node -> Maybe a) -> a -> [Token doc enr Node clip token] -> a
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



