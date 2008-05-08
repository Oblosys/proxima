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

data Wrapped docLevel doc enr node clip token = 
    WrappedDocEdit   (EditDocument     docLevel doc enr node clip token)
  | WrappedEnrEdit   (EditEnrichedDoc  docLevel doc enr node clip token)
  | WrappedPresEdit  (EditPresentation docLevel doc enr node clip token)
  | WrappedLayEdit   (EditLayout docLevel doc enr node clip token)
  | WrappedArrEdit   (EditArrangement docLevel doc enr node clip token)
  | WrappedRenEdit   (EditRendering docLevel doc enr node clip token)

  | WrappedDocEdit'  (EditDocument' docLevel doc enr node clip token)
  | WrappedEnrEdit'  (EditEnrichedDoc'  docLevel doc enr node clip token)
  | WrappedPresEdit' (EditPresentation' docLevel doc enr node clip token)
  | WrappedLayEdit'  (EditLayout' docLevel doc enr node clip token)
  | WrappedArrEdit'  (EditArrangement' docLevel doc enr node clip token)
  | WrappedRenEdit'  (EditRendering' docLevel doc enr node clip token)


instance Show (Wrapped docLevel doc enr node clip token) where
  show w = "<Wrapped>"
    
class Wrapable editOp docLevel doc enr node clip token | editOp -> docLevel doc enr node clip token where
  wrap :: editOp -> Wrapped docLevel doc enr node clip token
  unwrap :: Wrapped docLevel doc enr node clip token -> editOp

cast = unwrap . wrap

instance Wrapable (EditDocument docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapDoc wrapped) = wrapped
  wrap e                 = WrappedDocEdit e
  unwrap (WrappedDocEdit e) = e
  unwrap wrapped            = WrapDoc wrapped

instance Wrapable (EditEnrichedDoc docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapEnr wrapped) = wrapped
  wrap e                 = WrappedEnrEdit e
  unwrap (WrappedEnrEdit e) = e
  unwrap wrapped            = WrapEnr wrapped

instance Wrapable (EditPresentation  docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapPres wrapped) = wrapped
  wrap e                  = WrappedPresEdit e
  unwrap (WrappedPresEdit e) = e
  unwrap wrapped             = WrapPres wrapped

instance Wrapable (EditLayout docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapLay wrapped) = wrapped
  wrap e                 = WrappedLayEdit e
  unwrap (WrappedLayEdit e) = e
  unwrap wrapped            = WrapLay wrapped

instance Wrapable (EditArrangement docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapArr wrapped) = wrapped
  wrap e                 = WrappedArrEdit e
  unwrap (WrappedArrEdit e) = e
  unwrap wrapped            = WrapArr wrapped

instance Wrapable (EditRendering docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapRen wrapped) = wrapped
  wrap e                 = WrappedRenEdit e
  unwrap (WrappedRenEdit e) = e
  unwrap wrapped            = WrapRen wrapped

instance Wrapable (EditDocument' docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapDoc' wrapped) = wrapped
  wrap e                 = WrappedDocEdit' e
  unwrap (WrappedDocEdit' e) = e
  unwrap wrapped            = WrapDoc' wrapped

instance Wrapable (EditEnrichedDoc' docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapEnr' wrapped) = wrapped
  wrap e                 = WrappedEnrEdit' e
  unwrap (WrappedEnrEdit' e) = e
  unwrap wrapped            = WrapEnr' wrapped

instance Wrapable (EditPresentation'  docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapPres' wrapped) = wrapped
  wrap e                  = WrappedPresEdit' e
  unwrap (WrappedPresEdit' e) = e
  unwrap wrapped             = WrapPres' wrapped

instance Wrapable (EditLayout' docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapLay' wrapped) = wrapped
  wrap e                 = WrappedLayEdit' e
  unwrap (WrappedLayEdit' e) = e
  unwrap wrapped            = WrapLay' wrapped

instance Wrapable (EditArrangement' docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapArr' wrapped) = wrapped
  wrap e                 = WrappedArrEdit' e
  unwrap (WrappedArrEdit' e) = e
  unwrap wrapped            = WrapArr' wrapped

instance Wrapable (EditRendering' docLevel doc enr node clip token) docLevel doc enr node clip token where
  wrap (WrapRen' wrapped) = wrapped
  wrap e                 = WrappedRenEdit' e
  unwrap (WrappedRenEdit' e) = e
  unwrap wrapped            = WrapRen' wrapped

type EditDocument docLevel doc enr node clip token =
       EditDocument_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditDocument' docLevel doc enr node clip token =
       EditDocument'_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditEnrichedDoc docLevel doc enr node clip token =
       EditEnrichedDoc_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditEnrichedDoc' docLevel doc enr node clip token =
       EditEnrichedDoc'_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditPresentation docLevel doc enr node clip token =
       EditPresentation_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditPresentation' docLevel doc enr node clip token =
       EditPresentation'_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditLayout docLevel doc enr node clip token =
       EditLayout_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditLayout' docLevel doc enr node clip token =
       EditLayout'_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditArrangement docLevel doc enr node clip token =
       EditArrangement_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditArrangement' docLevel doc enr node clip token =
       EditArrangement'_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditRendering docLevel doc enr node clip token =
       EditRendering_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token

type EditRendering' docLevel doc enr node clip token =
       EditRendering'_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token


-- The type RenderingLevel contains GUICommand, which contains EditRendering. Hence, RenderingLevel
-- also gets all type parameters :-(

type RenderingLevel docLevel doc enr node clip token =
       RenderingLevel_ (Wrapped docLevel doc enr node clip token) docLevel doc enr node clip token