{-# LANGUAGE TemplateHaskell, KindSignatures, DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, FlexibleInstances, ConstraintKinds, MultiParamTypeClasses #-}

module Lamdu.Sugar.Types
    ( module Exported
    , EntityId
    , PaneBody(..), _PaneDefinition
    , Pane(..), paneBody, paneClose, paneMoveDown, paneMoveUp
    , TagPane(..), tpTag, tpTagData, tpSetTexts, tpSetSymbol
    , Repl(..), replExpr, replVarInfo, replResult
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
import           Hyper
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.Data.Tag as DataTag
import           Lamdu.I18N.LangId (LangId(..))
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval as Exported
import           Lamdu.Sugar.Types.Expression as Exported
import           Lamdu.Sugar.Types.GetVar as Exported
import           Lamdu.Sugar.Types.Parts as Exported
import           Lamdu.Sugar.Types.Simple as Exported
import           Lamdu.Sugar.Types.Tag as Exported
import           Lamdu.Sugar.Types.Type as Exported

import           Lamdu.Prelude

data DefinitionExpression name i o h = DefinitionExpression
    { _deType :: Scheme name
    , _dePresentationMode :: Maybe (i (Property o Meta.PresentationMode))
    , _deContent :: h :# Assignment name i o
    } deriving Generic

Lens.makeLenses ''DefinitionExpression

data DefinitionBuiltin name o = DefinitionBuiltin
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> o ()
    , _biType :: Scheme name
    } deriving Generic

data DefinitionBody name i o h
    = DefinitionBodyExpression (DefinitionExpression name i o h)
    | DefinitionBodyBuiltin (DefinitionBuiltin name o)
    deriving Generic

data Definition name i o h = Definition
    { _drName :: TagRef name i o
    , _drDefI :: V.Var
    , _drDefinitionState :: Property o Meta.DefinitionState
    , _drEntityId :: EntityId
    , _drBody :: DefinitionBody name i o h
    } deriving Generic

data TagPane name o = TagPane
    { _tpTag :: Tag name
    , _tpTagData :: DataTag.Tag
    , _tpSetSymbol :: DataTag.Symbol -> o ()
    , _tpSetTexts :: LangId -> DataTag.TextsInLang -> o ()
    } deriving Generic

data PaneBody name i o h
    = PaneDefinition (Definition name i o h)
    | PaneTag (TagPane name o)
    deriving Generic

data Pane name i o h = Pane
    { _paneBody :: PaneBody name i o h
    , _paneClose :: o EntityId
    , _paneMoveDown :: Maybe (o ())
    , _paneMoveUp :: Maybe (o ())
    } deriving Generic

data Repl name i o h = Repl
    { _replExpr :: h :# Binder name i o
    , _replVarInfo :: VarInfo
    , _replResult :: EvalCompletion name o
    } deriving Generic

Lens.makeLenses ''Repl

data WorkArea name i o h = WorkArea
    { _waPanes :: [Pane name i o h]
    , _waRepl :: Repl name i o h
    , _waGlobals :: i [NameRef name o]
    } deriving Generic

Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionBuiltin
Lens.makeLenses ''Pane
Lens.makeLenses ''TagPane
Lens.makeLenses ''WorkArea
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''PaneBody

traverse makeHTraversableAndBases
    [ ''DefinitionExpression, ''DefinitionBody, ''Definition
    , ''PaneBody, ''Pane
    , ''Repl, ''WorkArea
    ] <&> concat

instance RNodes (DefinitionExpression name i o)
instance RNodes (DefinitionBody name i o)
instance RNodes (Definition name i o)
instance RNodes (PaneBody name i o)
instance RNodes (Pane name i o)
instance RNodes (Repl name i o)
instance RNodes (WorkArea name i o)

type Dep c name i o =
    ( (c (Assignment name i o) :: Constraint)
    , c (Body name i o)
    , c (Binder name i o)
    , c (Const (BinderVarRef name o))
    , c (Const (NullaryVal name i o))
    , c (Const (GetVar name o))
    , c (Else name i o)
    , c (Function name i o)
    , c (DefinitionExpression name i o)
    , c (DefinitionBody name i o)
    , c (Definition name i o)
    , c (PaneBody name i o)
    , c (Pane name i o)
    , c (Repl name i o)
    , c (WorkArea name i o)
    )

instance Dep c name i o => Recursively c (DefinitionExpression name i o)
instance Dep c name i o => Recursively c (DefinitionBody name i o)
instance Dep c name i o => Recursively c (Definition name i o)
instance Dep c name i o => Recursively c (PaneBody name i o)
instance Dep c name i o => Recursively c (Pane name i o)
instance Dep c name i o => Recursively c (Repl name i o)
instance Dep c name i o => Recursively c (WorkArea name i o)
