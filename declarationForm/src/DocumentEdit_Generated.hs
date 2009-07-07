module DocumentEdit_Generated where

import Common.CommonTypes hiding (Dirty (..))
import Evaluation.DocTypes
import DocTypes_Generated
import DocUtils_Generated
import Evaluation.DocumentEdit
import Evaluation.DocUtils
import Presentation.PresTypes

----- GENERATED PART STARTS HERE. DO NOT EDIT ON OR BEYOND THIS LINE -----

--------------------------------------------------------------------------
-- Clip instance                                                        --
--------------------------------------------------------------------------

instance Clip ClipDoc where
  arityClip Clip_Nothing = -1
  arityClip (Clip_EnrichedDoc x) = arity x
  arityClip (Clip_Document x) = arity x
  arityClip (Clip_ChoiceDoc x) = arity x
  arityClip (Clip_Form x) = arity x
  arityClip (Clip_Expense x) = arity x
  arityClip (Clip_Currency x) = arity x
  arityClip (Clip_Tasks x) = arity x
  arityClip (Clip_Task x) = arity x
  arityClip (Clip_Description x) = arity x
  arityClip (Clip_List_Expense x) = arity x
  arityClip (Clip_List_Currency x) = arity x
  arityClip (Clip_List_Task x) = arity x
  arityClip (Clip_Bool x) = arity x
  arityClip (Clip_Int x) = arity x
  arityClip (Clip_String x) = arity x
  arityClip (Clip_Float x) = arity x

  alternativesClip Clip_Nothing = []
  alternativesClip (Clip_EnrichedDoc x) = alternatives x
  alternativesClip (Clip_Document x) = alternatives x
  alternativesClip (Clip_ChoiceDoc x) = alternatives x
  alternativesClip (Clip_Form x) = alternatives x
  alternativesClip (Clip_Expense x) = alternatives x
  alternativesClip (Clip_Currency x) = alternatives x
  alternativesClip (Clip_Tasks x) = alternatives x
  alternativesClip (Clip_Task x) = alternatives x
  alternativesClip (Clip_Description x) = alternatives x
  alternativesClip (Clip_List_Expense x) = alternatives x
  alternativesClip (Clip_List_Currency x) = alternatives x
  alternativesClip (Clip_List_Task x) = alternatives x
  alternativesClip (Clip_Bool x) = alternatives x
  alternativesClip (Clip_Int x) = alternatives x
  alternativesClip (Clip_String x) = alternatives x
  alternativesClip (Clip_Float x) = alternatives x

  holeClip Clip_Nothing = Clip_Nothing
  holeClip (Clip_EnrichedDoc x) = Clip_EnrichedDoc hole
  holeClip (Clip_Document x) = Clip_Document hole
  holeClip (Clip_ChoiceDoc x) = Clip_ChoiceDoc hole
  holeClip (Clip_Form x) = Clip_Form hole
  holeClip (Clip_Expense x) = Clip_Expense hole
  holeClip (Clip_Currency x) = Clip_Currency hole
  holeClip (Clip_Tasks x) = Clip_Tasks hole
  holeClip (Clip_Task x) = Clip_Task hole
  holeClip (Clip_Description x) = Clip_Description hole
  holeClip (Clip_List_Expense x) = Clip_List_Expense hole
  holeClip (Clip_List_Currency x) = Clip_List_Currency hole
  holeClip (Clip_List_Task x) = Clip_List_Task hole
  holeClip (Clip_Bool x) = Clip_Bool hole
  holeClip (Clip_Int x) = Clip_Int hole
  holeClip (Clip_String x) = Clip_String hole
  holeClip (Clip_Float x) = Clip_Float hole

  isListClip Clip_Nothing = False
  isListClip (Clip_EnrichedDoc x) = isList x
  isListClip (Clip_Document x) = isList x
  isListClip (Clip_ChoiceDoc x) = isList x
  isListClip (Clip_Form x) = isList x
  isListClip (Clip_Expense x) = isList x
  isListClip (Clip_Currency x) = isList x
  isListClip (Clip_Tasks x) = isList x
  isListClip (Clip_Task x) = isList x
  isListClip (Clip_Description x) = isList x
  isListClip (Clip_List_Expense x) = isList x
  isListClip (Clip_List_Currency x) = isList x
  isListClip (Clip_List_Task x) = isList x
  isListClip (Clip_Bool x) = isList x
  isListClip (Clip_Int x) = isList x
  isListClip (Clip_String x) = isList x
  isListClip (Clip_Float x) = isList x

  insertListClip i c Clip_Nothing = Clip_Nothing
  insertListClip i c (Clip_EnrichedDoc x) = insertList i c x
  insertListClip i c (Clip_Document x) = insertList i c x
  insertListClip i c (Clip_ChoiceDoc x) = insertList i c x
  insertListClip i c (Clip_Form x) = insertList i c x
  insertListClip i c (Clip_Expense x) = insertList i c x
  insertListClip i c (Clip_Currency x) = insertList i c x
  insertListClip i c (Clip_Tasks x) = insertList i c x
  insertListClip i c (Clip_Task x) = insertList i c x
  insertListClip i c (Clip_Description x) = insertList i c x
  insertListClip i c (Clip_List_Expense x) = insertList i c x
  insertListClip i c (Clip_List_Currency x) = insertList i c x
  insertListClip i c (Clip_List_Task x) = insertList i c x
  insertListClip i c (Clip_Bool x) = insertList i c x
  insertListClip i c (Clip_Int x) = insertList i c x
  insertListClip i c (Clip_String x) = insertList i c x
  insertListClip i c (Clip_Float x) = insertList i c x

  removeListClip i Clip_Nothing = Clip_Nothing
  removeListClip i (Clip_EnrichedDoc x) = removeList i x
  removeListClip i (Clip_Document x) = removeList i x
  removeListClip i (Clip_ChoiceDoc x) = removeList i x
  removeListClip i (Clip_Form x) = removeList i x
  removeListClip i (Clip_Expense x) = removeList i x
  removeListClip i (Clip_Currency x) = removeList i x
  removeListClip i (Clip_Tasks x) = removeList i x
  removeListClip i (Clip_Task x) = removeList i x
  removeListClip i (Clip_Description x) = removeList i x
  removeListClip i (Clip_List_Expense x) = removeList i x
  removeListClip i (Clip_List_Currency x) = removeList i x
  removeListClip i (Clip_List_Task x) = removeList i x
  removeListClip i (Clip_Bool x) = removeList i x
  removeListClip i (Clip_Int x) = removeList i x
  removeListClip i (Clip_String x) = removeList i x
  removeListClip i (Clip_Float x) = removeList i x




--------------------------------------------------------------------------
-- Editable instances                                                   --
--------------------------------------------------------------------------

instance Editable EnrichedDoc Document Node ClipDoc UserToken where
  select [] x = Clip_EnrichedDoc x
  select (0:p) (RootEnr x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_EnrichedDoc c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on EnrichedDoc") x
  paste (0:p) c (RootEnr x0) = RootEnr (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("RootEnr {ChoiceDoc} "  , Clip_EnrichedDoc $ RootEnr hole)
                   ,("{EnrichedDoc}", Clip_EnrichedDoc hole)
                   ]

  arity (RootEnr x0) = 1
  arity _                        = 0

  toClip t = Clip_EnrichedDoc t

  fromClip (Clip_EnrichedDoc t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrEnrichedDoc

  hole = HoleEnrichedDoc

  holeNodeConstr = Node_HoleEnrichedDoc

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Document Document Node ClipDoc UserToken where
  select [] x = Clip_Document x
  select (0:p) (RootDoc x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Document c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Document") x
  paste (0:p) c (RootDoc x0) = RootDoc (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("RootDoc {ChoiceDoc} "  , Clip_Document $ RootDoc hole)
                   ,("{Document}", Clip_Document hole)
                   ]

  arity (RootDoc x0) = 1
  arity _                        = 0

  toClip t = Clip_Document t

  fromClip (Clip_Document t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrDocument

  hole = HoleDocument

  holeNodeConstr = Node_HoleDocument

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable ChoiceDoc Document Node ClipDoc UserToken where
  select [] x = Clip_ChoiceDoc x
  select (0:p) (FormDoc x0) = select p x0
  select (0:p) (TaskDoc x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_ChoiceDoc c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on ChoiceDoc") x
  paste (0:p) c (FormDoc x0) = FormDoc (paste p c x0)
  paste (0:p) c (TaskDoc x0) = TaskDoc (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("FormDoc {Form} "  , Clip_ChoiceDoc $ FormDoc hole)
                   , ("TaskDoc {Tasks} "  , Clip_ChoiceDoc $ TaskDoc hole)
                   ,("{ChoiceDoc}", Clip_ChoiceDoc hole)
                   ]

  arity (FormDoc x0) = 1
  arity (TaskDoc x0) = 1
  arity _                        = 0

  toClip t = Clip_ChoiceDoc t

  fromClip (Clip_ChoiceDoc t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrChoiceDoc

  hole = HoleChoiceDoc

  holeNodeConstr = Node_HoleChoiceDoc

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Form Document Node ClipDoc UserToken where
  select [] x = Clip_Form x
  select (0:p) (Form x0 x1 x2 x3) = select p x0
  select (1:p) (Form x0 x1 x2 x3) = select p x1
  select (2:p) (Form x0 x1 x2 x3) = select p x2
  select (3:p) (Form x0 x1 x2 x3) = select p x3
  select _ _ = Clip_Nothing

  paste [] (Clip_Form c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Form") x
  paste (0:p) c (Form x0 x1 x2 x3) = Form (paste p c x0) x1 x2 x3
  paste (1:p) c (Form x0 x1 x2 x3) = Form x0 (paste p c x1) x2 x3
  paste (2:p) c (Form x0 x1 x2 x3) = Form x0 x1 (paste p c x2) x3
  paste (3:p) c (Form x0 x1 x2 x3) = Form x0 x1 x2 (paste p c x3)
  paste _ _ x = x

  alternatives _ = [ ("Form {String} {String} {List_Expense} {List_Currency} "  , Clip_Form $ Form hole hole hole hole)
                   ,("{Form}", Clip_Form hole)
                   ]

  arity (Form x0 x1 x2 x3) = 4
  arity _                        = 0

  toClip t = Clip_Form t

  fromClip (Clip_Form t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrForm

  hole = HoleForm

  holeNodeConstr = Node_HoleForm

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Expense Document Node ClipDoc UserToken where
  select [] x = Clip_Expense x
  select (0:p) (Expense x0 x1 x2) = select p x0
  select (1:p) (Expense x0 x1 x2) = select p x1
  select (2:p) (Expense x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Expense c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Expense") x
  paste (0:p) c (Expense x0 x1 x2) = Expense (paste p c x0) x1 x2
  paste (1:p) c (Expense x0 x1 x2) = Expense x0 (paste p c x1) x2
  paste (2:p) c (Expense x0 x1 x2) = Expense x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("Expense {String} {Float} {Int} "  , Clip_Expense $ Expense hole hole hole)
                   ,("{Expense}", Clip_Expense hole)
                   ]

  arity (Expense x0 x1 x2) = 3
  arity _                        = 0

  toClip t = Clip_Expense t

  fromClip (Clip_Expense t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrExpense

  hole = HoleExpense

  holeNodeConstr = Node_HoleExpense

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Currency Document Node ClipDoc UserToken where
  select [] x = Clip_Currency x
  select (0:p) (Currency x0 x1) = select p x0
  select (1:p) (Currency x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Currency c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Currency") x
  paste (0:p) c (Currency x0 x1) = Currency (paste p c x0) x1
  paste (1:p) c (Currency x0 x1) = Currency x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Currency {String} {Float} "  , Clip_Currency $ Currency hole hole)
                   ,("{Currency}", Clip_Currency hole)
                   ]

  arity (Currency x0 x1) = 2
  arity _                        = 0

  toClip t = Clip_Currency t

  fromClip (Clip_Currency t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrCurrency

  hole = HoleCurrency

  holeNodeConstr = Node_HoleCurrency

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Tasks Document Node ClipDoc UserToken where
  select [] x = Clip_Tasks x
  select (0:p) (Tasks x0 x1) = select p x0
  select (1:p) (Tasks x0 x1) = select p x1
  select _ _ = Clip_Nothing

  paste [] (Clip_Tasks c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Tasks") x
  paste (0:p) c (Tasks x0 x1) = Tasks (paste p c x0) x1
  paste (1:p) c (Tasks x0 x1) = Tasks x0 (paste p c x1)
  paste _ _ x = x

  alternatives _ = [ ("Tasks {Bool} {List_Task} "  , Clip_Tasks $ Tasks hole hole)
                   ,("{Tasks}", Clip_Tasks hole)
                   ]

  arity (Tasks x0 x1) = 2
  arity _                        = 0

  toClip t = Clip_Tasks t

  fromClip (Clip_Tasks t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrTasks

  hole = HoleTasks

  holeNodeConstr = Node_HoleTasks

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Task Document Node ClipDoc UserToken where
  select [] x = Clip_Task x
  select (0:p) (BasicTask x0 x1) = select p x0
  select (1:p) (BasicTask x0 x1) = select p x1
  select (0:p) (CompositeTask x0 x1 x2) = select p x0
  select (1:p) (CompositeTask x0 x1 x2) = select p x1
  select (2:p) (CompositeTask x0 x1 x2) = select p x2
  select _ _ = Clip_Nothing

  paste [] (Clip_Task c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Task") x
  paste (0:p) c (BasicTask x0 x1) = BasicTask (paste p c x0) x1
  paste (1:p) c (BasicTask x0 x1) = BasicTask x0 (paste p c x1)
  paste (0:p) c (CompositeTask x0 x1 x2) = CompositeTask (paste p c x0) x1 x2
  paste (1:p) c (CompositeTask x0 x1 x2) = CompositeTask x0 (paste p c x1) x2
  paste (2:p) c (CompositeTask x0 x1 x2) = CompositeTask x0 x1 (paste p c x2)
  paste _ _ x = x

  alternatives _ = [ ("BasicTask {Description} {Bool} "  , Clip_Task $ BasicTask hole hole)
                   , ("CompositeTask {Bool} {Description} {List_Task} "  , Clip_Task $ CompositeTask hole hole hole)
                   ,("{Task}", Clip_Task hole)
                   ]

  arity (BasicTask x0 x1) = 2
  arity (CompositeTask x0 x1 x2) = 3
  arity _                        = 0

  toClip t = Clip_Task t

  fromClip (Clip_Task t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrTask

  hole = HoleTask

  holeNodeConstr = Node_HoleTask

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Description Document Node ClipDoc UserToken where
  select [] x = Clip_Description x
  select (0:p) (Description x0) = select p x0
  select _ _ = Clip_Nothing

  paste [] (Clip_Description c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on Description") x
  paste (0:p) c (Description x0) = Description (paste p c x0)
  paste _ _ x = x

  alternatives _ = [ ("Description {String} "  , Clip_Description $ Description hole)
                   ,("{Description}", Clip_Description hole)
                   ]

  arity (Description x0) = 1
  arity _                        = 0

  toClip t = Clip_Description t

  fromClip (Clip_Description t) = Just t
  fromClip _             = Nothing

  parseErr = ParseErrDescription

  hole = HoleDescription

  holeNodeConstr = Node_HoleDescription

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable List_Expense Document Node ClipDoc UserToken where
  select [] x = Clip_List_Expense x
  select (n:p) (List_Expense cxs) =
    let xs = fromConsList_Expense cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Expense c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Expense")   x
  paste (n:p) c (List_Expense cxs) =
    let xs = fromConsList_Expense cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Expense (replaceList_Expense n x' cxs)
        else List_Expense cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Expense}", Clip_List_Expense hole)
                   ]

  arity (List_Expense x1) = length (fromConsList_Expense x1)
  arity _ = 0

  toClip t = Clip_List_Expense t

  fromClip (Clip_List_Expense t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Expense

  hole = List_Expense Nil_Expense

  holeNodeConstr = Node_HoleList_Expense

  isList _ = True

  insertList n (Clip_Expense c) (List_Expense cxs) = Clip_List_Expense $ List_Expense (insertList_Expense n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Expense xs
  insertList _ c xs = Clip_List_Expense xs

  removeList n (List_Expense cxs) = Clip_List_Expense $ List_Expense (removeList_Expense n cxs)
  removeList _ xs = Clip_List_Expense $ xs

instance Editable List_Currency Document Node ClipDoc UserToken where
  select [] x = Clip_List_Currency x
  select (n:p) (List_Currency cxs) =
    let xs = fromConsList_Currency cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Currency c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Currency")   x
  paste (n:p) c (List_Currency cxs) =
    let xs = fromConsList_Currency cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Currency (replaceList_Currency n x' cxs)
        else List_Currency cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Currency}", Clip_List_Currency hole)
                   ]

  arity (List_Currency x1) = length (fromConsList_Currency x1)
  arity _ = 0

  toClip t = Clip_List_Currency t

  fromClip (Clip_List_Currency t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Currency

  hole = List_Currency Nil_Currency

  holeNodeConstr = Node_HoleList_Currency

  isList _ = True

  insertList n (Clip_Currency c) (List_Currency cxs) = Clip_List_Currency $ List_Currency (insertList_Currency n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Currency xs
  insertList _ c xs = Clip_List_Currency xs

  removeList n (List_Currency cxs) = Clip_List_Currency $ List_Currency (removeList_Currency n cxs)
  removeList _ xs = Clip_List_Currency $ xs

instance Editable List_Task Document Node ClipDoc UserToken where
  select [] x = Clip_List_Task x
  select (n:p) (List_Task cxs) =
    let xs = fromConsList_Task cxs
    in  if n < length xs 
        then select p (xs !! n)
        else Clip_Nothing
  select _ _ = Clip_Nothing

  paste [] (Clip_List_Task c) _ = c
  paste [] c x = debug Err ("Type error: pasting "++show c++" on List_Task")   x
  paste (n:p) c (List_Task cxs) =
    let xs = fromConsList_Task cxs
    in  if n < length xs
        then let x  = xs!!n
                 x' = paste p c x
             in  List_Task (replaceList_Task n x' cxs)
        else List_Task cxs -- paste beyond end of list
  paste _ _ x = x

  alternatives _ = [("{List_Task}", Clip_List_Task hole)
                   ]

  arity (List_Task x1) = length (fromConsList_Task x1)
  arity _ = 0

  toClip t = Clip_List_Task t

  fromClip (Clip_List_Task t) = Just t
  fromClip _ = Nothing

  parseErr = ParseErrList_Task

  hole = List_Task Nil_Task

  holeNodeConstr = Node_HoleList_Task

  isList _ = True

  insertList n (Clip_Task c) (List_Task cxs) = Clip_List_Task $ List_Task (insertList_Task n c cxs)
  insertList _ _ xs = debug Err "Type error, no paste" $ Clip_List_Task xs
  insertList _ c xs = Clip_List_Task xs

  removeList n (List_Task cxs) = Clip_List_Task $ List_Task (removeList_Task n cxs)
  removeList _ xs = Clip_List_Task $ xs




--------------------------------------------------------------------------
-- Editable instances for Document, EnrichedDoc and primitive types     --
--------------------------------------------------------------------------

instance Editable Int Document Node ClipDoc UserToken where
  select [] x = Clip_Int x
  select _  _ = Clip_Nothing
  paste [] (Clip_Int c) x = c
  paste [] c            x =  debug Err ("Type error: pasting "++show c++" on Int") x
  paste _  _             x = x
  
  alternatives _ = [ ("0", Clip_Int 0) ]
  
  arity _ = 0

  toClip t = Clip_Int t

  fromClip (Clip_Int t) = Just t
  fromClip _            = Nothing

  parseErr _ = 0

  hole = 0

  holeNodeConstr = error "Type Int is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Float Document Node ClipDoc UserToken where
  select [] x = Clip_Float x
  select _  _ = Clip_Nothing
  paste [] (Clip_Float c) x = c
  paste [] c              x =  debug Err ("Type error: pasting "++show c++" on Float") x
  paste _  _              x = x
  
  alternatives _ = [ ("0.0", Clip_Float 0.0) ]
  
  arity _ = 0

  toClip t = Clip_Float t

  fromClip (Clip_Float t) = Just t
  fromClip _              = Nothing

  parseErr _ = 0

  hole = 0

  holeNodeConstr = error "Type Float is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable Bool Document Node ClipDoc UserToken where
  select [] x = Clip_Bool x                            
  select _  _ = Clip_Nothing                           
  paste [] (Clip_Bool c) x = c                         
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on Bool") x
  paste _  _             x = x
  alternatives _ = [ ("True", Clip_Bool True)        
                   , ("False", Clip_Bool False)      
                   ]    
  arity _ = 0                                          

  toClip t = Clip_Bool t

  fromClip (Clip_Bool t) = Just t
  fromClip _             = Nothing

  parseErr _ = False

  hole = False

  holeNodeConstr = error "Type Bool is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing

instance Editable String Document Node ClipDoc UserToken where
  select [] x = Clip_String x
  select _  _ = Clip_Nothing
  paste [] (Clip_String c) x = c
  paste [] c             x =  debug Err ("Type error: pasting "++show c++" on String") x
  paste _  _             x = x

  alternatives _ = [ ("string", Clip_String "string")
                   ] 
 
  arity _ = 0

  toClip t = Clip_String t

  fromClip (Clip_String t) = Just t
  fromClip _               = Nothing

  parseErr _ = "{ParseErr}"

  hole = "{String}"

  holeNodeConstr = error "Type String is primitive and has no hole node constructorstructor"

  isList _ = False
  insertList _ _ _ = Clip_Nothing
  removeList _ _ = Clip_Nothing


