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
    , DefinitionU
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import           Data.UUID.Types (UUID)
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

data DefinitionExpression name m expr = DefinitionExpression
    { _deType :: Scheme name
    , _dePresentationMode :: Maybe (m (Property m Meta.PresentationMode))
    , _deContent :: Binder name m expr
    } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin name m = DefinitionBuiltin
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> m ()
    , _biType :: Scheme name
    }

data DefinitionBody name m expr
    = DefinitionBodyExpression (DefinitionExpression name m expr)
    | DefinitionBodyBuiltin (DefinitionBuiltin name m)
    deriving (Functor, Foldable, Traversable)

data Definition name m expr = Definition
    { _drName :: Tag name m
    , _drDefI :: V.Var
    , _drDefinitionState :: m (Property m Meta.DefinitionState)
    , _drEntityId :: EntityId
    , _drBody :: DefinitionBody name m expr
    } deriving (Functor, Foldable, Traversable)

type DefinitionU m a = Definition UUID m (Expression UUID m a)

data Pane name m a = Pane
    { _paneDefinition :: Definition name m (Expression name m a)
    , _paneClose :: m EntityId
    , _paneMoveDown :: Maybe (m ())
    , _paneMoveUp :: Maybe (m ())
    } deriving (Functor, Foldable, Traversable)

data WorkArea name m a = WorkArea
    { _waPanes :: [Pane name m a]
    , _waRepl :: Expression name m a
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionBuiltin
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''Pane
Lens.makeLenses ''WorkArea
Lens.makePrisms ''DefinitionBody
