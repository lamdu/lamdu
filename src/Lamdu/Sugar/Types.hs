{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types
    ( module Exported
    , EntityId
    , Pane(..), paneDefinition, paneClose, paneMoveDown, paneMoveUp
    , Repl(..), replExpr, replResult
    , WorkArea(..), waPanes, waRepl, waGlobals
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
import           Lamdu.Sugar.Types.Eval as Exported
import           Lamdu.Sugar.Types.Expression as Exported
import           Lamdu.Sugar.Types.GetVar as Exported
import           Lamdu.Sugar.Types.Hole as Exported
import           Lamdu.Sugar.Types.Parts as Exported
import           Lamdu.Sugar.Types.Simple as Exported
import           Lamdu.Sugar.Types.Tag as Exported
import           Lamdu.Sugar.Types.Type as Exported

import           Lamdu.Prelude

data DefinitionExpression name i o expr = DefinitionExpression
    { _deType :: Scheme name
    , _dePresentationMode :: Maybe (i (Property o Meta.PresentationMode))
    , _deContent :: Assignment name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data DefinitionBuiltin name o = DefinitionBuiltin
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> o ()
    , _biType :: Scheme name
    } deriving Generic

data DefinitionBody name i o expr
    = DefinitionBodyExpression (DefinitionExpression name i o expr)
    | DefinitionBodyBuiltin (DefinitionBuiltin name o)
    deriving (Functor, Foldable, Traversable, Generic)

data Definition name i o expr = Definition
    { _drName :: Tag name i o
    , _drDefI :: V.Var
    , _drDefinitionState :: i (Property o Meta.DefinitionState)
    , _drEntityId :: EntityId
    , _drBody :: DefinitionBody name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data Pane name i o a = Pane
    { _paneDefinition :: Definition name i o (Expression name i o a)
    , _paneClose :: o EntityId
    , _paneMoveDown :: Maybe (o ())
    , _paneMoveUp :: Maybe (o ())
    } deriving (Functor, Foldable, Traversable, Generic)

data Repl name i o a = Repl
    { _replExpr :: Expression name i o a
    , _replResult :: EvalCompletion name o
    } deriving (Functor, Foldable, Traversable, Generic)

data WorkArea name i o a = WorkArea
    { _waPanes :: [Pane name i o a]
    , _waRepl :: Repl name i o a
    , _waGlobals :: i [NameRef name o]
    } deriving (Functor, Foldable, Traversable, Generic)

Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionBuiltin
Lens.makeLenses ''DefinitionExpression
Lens.makeLenses ''Pane
Lens.makeLenses ''Repl
Lens.makeLenses ''WorkArea
Lens.makePrisms ''DefinitionBody
