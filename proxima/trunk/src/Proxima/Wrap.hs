-----------------------------------------------------------------------------------------
{-| Module      : Proxima.Wrap
    Copyright   : (c) 2007 Martijn Schrage
    License     : All Rights Reserved

    Maintainer  : martijn@cs.uu.nl
    Stability   : experimental
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Proxima.Wrap where

import Evaluation.DocTypes
import Evaluation.EnrTypes
import Presentation.PresTypes
import Layout.LayTypes
import Arrangement.ArrTypes
import Rendering.RenTypes

{-
Wrap implements wrapped editops



instead of returning e.g. :: EditArrangement, return a wrap $ ..
wrap :: edit op returns a wrapped editop.
translate and present functions unwrap it, so it arrives at its destination.
cast.

each layer component takes edit op -> edit ops.



-}

data Wrapped doc enr node clip token = 
    WrappedDocEdit   (EditDocument     doc enr node clip token)
  | WrappedEnrEdit   (EditEnrichedDoc  doc enr node clip token)
  | WrappedPresEdit  (EditPresentation doc enr node clip token)
  | WrappedLayEdit   (EditLayout doc enr node clip token)
  | WrappedArrEdit   (EditArrangement doc enr node clip token)
  | WrappedRenEdit   (EditRendering doc enr node clip token)

  | WrappedDocEdit'  (EditDocument' doc enr node clip token)
  | WrappedEnrEdit'  (EditEnrichedDoc'  doc enr node clip token)
  | WrappedPresEdit' (EditPresentation' doc enr node clip token)
  | WrappedLayEdit'  (EditLayout' doc enr node clip token)
  | WrappedArrEdit'  (EditArrangement' doc enr node clip token)
  | WrappedRenEdit'  (EditRendering' doc enr node clip token)


instance Show (Wrapped doc enr node clip token) where
  show w = "<Wrapped>"
    
class Wrapable editOp doc enr node clip token | editOp -> doc enr node clip token where
  wrap :: editOp -> Wrapped doc enr node clip token
  unwrap :: Wrapped doc enr node clip token -> editOp

cast = unwrap . wrap

instance Wrapable (EditDocument doc enr node clip token) doc enr node clip token where
  wrap (WrapDoc wrapped) = wrapped
  wrap e                 = WrappedDocEdit e
  unwrap (WrappedDocEdit e) = e
  unwrap wrapped            = WrapDoc wrapped

instance Wrapable (EditEnrichedDoc doc enr node clip token) doc enr node clip token where
  wrap (WrapEnr wrapped) = wrapped
  wrap e                 = WrappedEnrEdit e
  unwrap (WrappedEnrEdit e) = e
  unwrap wrapped            = WrapEnr wrapped

instance Wrapable (EditPresentation  doc enr node clip token) doc enr node clip token where
  wrap (WrapPres wrapped) = wrapped
  wrap e                  = WrappedPresEdit e
  unwrap (WrappedPresEdit e) = e
  unwrap wrapped             = WrapPres wrapped

instance Wrapable (EditLayout doc enr node clip token) doc enr node clip token where
  wrap (WrapLay wrapped) = wrapped
  wrap e                 = WrappedLayEdit e
  unwrap (WrappedLayEdit e) = e
  unwrap wrapped            = WrapLay wrapped

instance Wrapable (EditArrangement doc enr node clip token) doc enr node clip token where
  wrap (WrapArr wrapped) = wrapped
  wrap e                 = WrappedArrEdit e
  unwrap (WrappedArrEdit e) = e
  unwrap wrapped            = WrapArr wrapped

instance Wrapable (EditRendering doc enr node clip token) doc enr node clip token where
  wrap (WrapRen wrapped) = wrapped
  wrap e                 = WrappedRenEdit e
  unwrap (WrappedRenEdit e) = e
  unwrap wrapped            = WrapRen wrapped

instance Wrapable (EditDocument' doc enr node clip token) doc enr node clip token where
  wrap (WrapDoc' wrapped) = wrapped
  wrap e                 = WrappedDocEdit' e
  unwrap (WrappedDocEdit' e) = e
  unwrap wrapped            = WrapDoc' wrapped

instance Wrapable (EditEnrichedDoc' doc enr node clip token) doc enr node clip token where
  wrap (WrapEnr' wrapped) = wrapped
  wrap e                 = WrappedEnrEdit' e
  unwrap (WrappedEnrEdit' e) = e
  unwrap wrapped            = WrapEnr' wrapped

instance Wrapable (EditPresentation'  doc enr node clip token) doc enr node clip token where
  wrap (WrapPres' wrapped) = wrapped
  wrap e                  = WrappedPresEdit' e
  unwrap (WrappedPresEdit' e) = e
  unwrap wrapped             = WrapPres' wrapped

instance Wrapable (EditLayout' doc enr node clip token) doc enr node clip token where
  wrap (WrapLay' wrapped) = wrapped
  wrap e                 = WrappedLayEdit' e
  unwrap (WrappedLayEdit' e) = e
  unwrap wrapped            = WrapLay' wrapped

instance Wrapable (EditArrangement' doc enr node clip token) doc enr node clip token where
  wrap (WrapArr' wrapped) = wrapped
  wrap e                 = WrappedArrEdit' e
  unwrap (WrappedArrEdit' e) = e
  unwrap wrapped            = WrapArr' wrapped

instance Wrapable (EditRendering' doc enr node clip token) doc enr node clip token where
  wrap (WrapRen' wrapped) = wrapped
  wrap e                 = WrappedRenEdit' e
  unwrap (WrappedRenEdit' e) = e
  unwrap wrapped            = WrapRen' wrapped

type EditDocument doc enr node clip token =
       EditDocument_ (Wrapped doc enr node clip token) doc enr node clip token

type EditDocument' doc enr node clip token =
       EditDocument'_ (Wrapped doc enr node clip token) doc enr node clip token

type EditEnrichedDoc doc enr node clip token =
       EditEnrichedDoc_ (Wrapped doc enr node clip token) doc enr node clip token

type EditEnrichedDoc' doc enr node clip token =
       EditEnrichedDoc'_ (Wrapped doc enr node clip token) doc enr node clip token

type EditPresentation doc enr node clip token =
       EditPresentation_ (Wrapped doc enr node clip token) doc enr node clip token

type EditPresentation' doc enr node clip token =
       EditPresentation'_ (Wrapped doc enr node clip token) doc enr node clip token

type EditLayout doc enr node clip token =
       EditLayout_ (Wrapped doc enr node clip token) doc enr node clip token

type EditLayout' doc enr node clip token =
       EditLayout'_ (Wrapped doc enr node clip token) doc enr node clip token

type EditArrangement doc enr node clip token =
       EditArrangement_ (Wrapped doc enr node clip token) doc enr node clip token

type EditArrangement' doc enr node clip token =
       EditArrangement'_ (Wrapped doc enr node clip token) doc enr node clip token

type EditRendering doc enr node clip token =
       EditRendering_ (Wrapped doc enr node clip token) doc enr node clip token

type EditRendering' doc enr node clip token =
       EditRendering'_ (Wrapped doc enr node clip token) doc enr node clip token


-- The type RenderingLevel contains GUICommand, which contains EditRendering. Hence, RenderingLevel
-- also gets all type parameters :-(

type RenderingLevel doc enr node clip token =
       RenderingLevel_ (Wrapped doc enr node clip token) doc enr node clip token
       
castRemainingEditOps :: ( Wrapable editOp doc enbr node clip token
                        , Wrapable editOp' doc enbr node clip token ) =>
                        (editOp -> IO ([editOp'],state,level)) -> [editOp] -> IO ([editOp'], state, level)
castRemainingEditOps presOrTrans (editOp:editOps) =
 do { (editOps',state,level) <- presOrTrans editOp
    ; return (editOps' ++ map cast editOps, state, level)
    }

castRemainingEditOpsRedirect :: ( Wrapable editOp doc enbr node clip token
                                , Wrapable editOp' doc enbr node clip token ) =>
                                [editOp] -> (editOp -> editOp') -> [editOp']
castRemainingEditOpsRedirect (editOp:editOps) redirect = redirect editOp : map cast editOps
