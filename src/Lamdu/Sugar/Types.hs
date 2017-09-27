{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable #-}
module Lamdu.Sugar.Types
    ( module Exported
    , EntityId
    , Pane(..), paneDefinition, paneClose, paneMoveDown, paneMoveUp
    , WorkArea(..), waPanes, waRepl
    , Definition(..), drDefinitionState, drEntityId, drName, drBody, drDefI
    , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
    , DefinitionExpression(..), deContent, deType
    , Meta.SpecialArgs(..), Meta.PresentationMode
    , Meta.DefinitionState(..)
    , DefinitionBuiltin(..), biType, biName, biSetName
    , DefinitionU
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Property (Property)
import           Data.UUID.Types (UUID)
import           Lamdu.Calc.Type.Scheme (Scheme)
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder as Exported
import           Lamdu.Sugar.Types.Expression as Exported
import           Lamdu.Sugar.Types.GetVar as Exported
import           Lamdu.Sugar.Types.Hole as Exported

import           Lamdu.Prelude

data DefinitionExpression name m expr = DefinitionExpression
    { _deType :: Scheme
    , _deContent :: Binder name m expr
    } deriving (Functor, Foldable, Traversable)

data DefinitionBuiltin m = DefinitionBuiltin
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> m ()
    , _biType :: Scheme
    }

data DefinitionBody name m expr
    = DefinitionBodyExpression (DefinitionExpression name m expr)
    | DefinitionBodyBuiltin (DefinitionBuiltin m)
    deriving (Functor, Foldable, Traversable)

data Definition name m expr = Definition
    { _drName :: name
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
