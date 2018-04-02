{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types
    ( module Exported
    , EntityId
    , Pane(..), paneDefinition, paneClose, paneMoveDown, paneMoveUp
    , WorkArea(..), waPanes, waRepl
    , Definition(..), drDefinitionState, drEntityId, drName, drBody, drDefI
    , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
    , DefinitionExpression(..), deContent, dePresentationMode, deType
    , Meta.SpecialArgs(..), Meta.PresentationMode
    , Meta.DefinitionState(..)
    , DefinitionBuiltin(..), biType, biName, biSetName
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder as Exported
import           Lamdu.Sugar.Types.Eval as Exported
import           Lamdu.Sugar.Types.Expression as Exported
import           Lamdu.Sugar.Types.GetVar as Exported
import           Lamdu.Sugar.Types.Hole as Exported
import           Lamdu.Sugar.Types.Tag as Exported
import           Lamdu.Sugar.Types.Type as Exported

import           Lamdu.Prelude

data DefinitionExpression name im am expr = DefinitionExpression
    { _deType :: Scheme name
    , _dePresentationMode :: Maybe (im (Property am Meta.PresentationMode))
    , _deContent :: Binder name im am expr
    } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin name am = DefinitionBuiltin
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> am ()
    , _biType :: Scheme name
    }

data DefinitionBody name im am expr
    = DefinitionBodyExpression (DefinitionExpression name im am expr)
    | DefinitionBodyBuiltin (DefinitionBuiltin name am)
    deriving (Functor, Foldable, Traversable)

data Definition name im am expr = Definition
    { _drName :: Tag name im am
    , _drDefI :: V.Var
    , _drDefinitionState :: im (Property am Meta.DefinitionState)
    , _drEntityId :: EntityId
    , _drBody :: DefinitionBody name im am expr
    } deriving (Functor, Foldable, Traversable)

data Pane name im am a = Pane
    { _paneDefinition :: Definition name im am (Expression name im am a)
    , _paneClose :: am EntityId
    , _paneMoveDown :: Maybe (am ())
    , _paneMoveUp :: Maybe (am ())
    } deriving (Functor, Foldable, Traversable)

data WorkArea name im am a = WorkArea
    { _waPanes :: [Pane name im am a]
    , _waRepl :: Expression name im am a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionBuiltin
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''Pane
Lens.makeLenses ''WorkArea
Lens.makePrisms ''DefinitionBody
