module Architecture where

import ArchitectureLibM

{- The Proxima modules that contain the actual presentation and translation functions:

import qualified Presenter
import qualified Arranger
import qualified UnArranger
import qualified ArrangementDefs
import qualified Renderer
import qualified GestureInterpreter
import qualified RenderingDefs


import Graphics.UI.ObjectIO
-}

type Step m nStep a b c d = (a -> m (b, nStep m c d a b))

newtype TransStep m a b c d = 
            TransStep {transStep :: Step m PresStep a b c d}

newtype PresStep m a b c d = 
            PresStep {presStep :: Step m TransStep a b c d}


instance Pack m (TransStep m a b c d) a b (PresStep m c d a b) where
  pack = TransStep
  unpack = transStep

instance Pack m (PresStep m a b c d) a b (TransStep m c d a b) where
  pack = PresStep
  unpack = presStep

lift :: Monad m => Simple m state doc pres editDoc editPres editDoc' editPres' -> (state, doc) 
                -> TransStep m (pres, editPres) (doc, editDoc) (doc,editDoc') (pres, editPres') 
lift simple =
  fix $ liftStep (translate simple) 
      . liftStep (present simple) 

combine :: Monad m => TransStep m editM editH dataH dataM -> TransStep m editL editM dataM dataL
                   -> TransStep m editL editH dataH dataL
combine upr lwr =  
  fix (combineStepUp . combineStepDown) upr lwr


--------------------

data Simple' m state doc pres editDoc editPres editDoc' editPres' =
       Simple' { translate' :: state -> pres -> doc -> editPres -> m (editDoc, state, pres)
               , present' ::   state -> doc -> pres -> editDoc' -> m (editPres', state, doc)
               }

data Simple m state doc pres editDoc editPres editDoc' editPres' =
       Simple { translate :: LayerFunction m (state, doc) (pres, editPres) (state, pres) (doc, editDoc)
              , present ::   LayerFunction m (state, pres) (doc, editDoc') (state, doc) (pres, editPres')
              }

wrap :: Monad m => Simple' m state doc pres editDoc editPres  editDoc' editPres'
                -> Simple m state doc pres editDoc editPres editDoc' editPres'
wrap (Simple' translate' present') = 
  Simple { translate = \(state, doc) (pres, editPres) ->
                         do { (editDoc, state, pres) <- translate' state pres doc editPres
                            ; return ((doc, editDoc), (state, pres))
                            }
         , present   = \(state, pres) (doc, editDoc') ->
                         do { (editPres', state, doc) <- present' state doc pres editDoc'
                            ; return ((pres, editPres'), (state, doc))
                            }
         }

{- These defs are commented because the functions come from the imported modules, and the Draw monad from ObjectIO

--lift layer function to Draw monad 
lDraw :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> Draw e
lDraw f = \a b c d -> return (f a b c d)

-- lift io layer function to draw monad
lIO :: (a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> Draw e
lIO f = \a b c d -> liftIO (f a b c d)




renderingLayer    = Simple' (lDraw GestureInterpreter.interpret) (lDraw Renderer.render)
arrangementLayer  = Simple' (lDraw UnArranger.unArrange) Arranger.arrange
presentationLayer = Simple' (lIO Presenter.parseIO) (lDraw Presenter.present)
--evaluationLayer   = Simple' Evaluator.evaluate  Reducer.reduce   

-}
