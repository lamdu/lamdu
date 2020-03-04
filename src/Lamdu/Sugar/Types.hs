{-# LANGUAGE TemplateHaskell #-}
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
import           Lamdu.Sugar.Types.Tag as Exported
import           Lamdu.Sugar.Types.Type as Exported

import           Lamdu.Prelude

data DefinitionExpression v name i o a = DefinitionExpression
    { _deType :: Scheme name
    , _dePresentationMode :: Maybe (i (Property o Meta.PresentationMode))
    , _deContent :: Annotated a # Assignment v name i o
    } deriving Generic

Lens.makeLenses ''DefinitionExpression

instance Functor (DefinitionExpression v name i o) where
    fmap f = deContent . hflipped %~ hmap (\_ -> Lens._Wrapped %~ f)

instance Foldable (DefinitionExpression v name i o) where
    foldMap f = (^. deContent . hflipped . Lens.to (hfoldMap (\_ (Const x) -> f x)))

instance Traversable (DefinitionExpression v name i o) where
    traverse f = deContent (htraverseFlipped (\_ -> Lens._Wrapped f))

data DefinitionBuiltin name o = DefinitionBuiltin
    { _biName :: Definition.FFIName
    , _biSetName :: Definition.FFIName -> o ()
    , _biType :: Scheme name
    } deriving Generic

data DefinitionBody v name i o a
    = DefinitionBodyExpression (DefinitionExpression v name i o a)
    | DefinitionBodyBuiltin (DefinitionBuiltin name o)
    deriving (Functor, Foldable, Traversable, Generic)

data Definition v name i o a = Definition
    { _drName :: TagRef name i o
    , _drDefI :: V.Var
    , _drDefinitionState :: Property o Meta.DefinitionState
    , _drEntityId :: EntityId
    , _drBody :: DefinitionBody v name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data TagPane name o = TagPane
    { _tpTag :: Tag name
    , _tpTagData :: DataTag.Tag
    , _tpSetSymbol :: DataTag.Symbol -> o ()
    , _tpSetTexts :: LangId -> DataTag.TextsInLang -> o ()
    } deriving Generic

data PaneBody v name i o a
    = PaneDefinition (Definition v name i o a)
    | PaneTag (TagPane name o)
    deriving (Functor, Foldable, Traversable, Generic)

data Pane name i o a = Pane
    { _paneBody :: PaneBody (EvaluationScopes name i) name i o a
    , _paneClose :: o EntityId
    , _paneMoveDown :: Maybe (o ())
    , _paneMoveUp :: Maybe (o ())
    } deriving (Functor, Foldable, Traversable, Generic)

data Repl name i o a = Repl
    { _replExpr :: Annotated a # Binder (EvaluationScopes name i) name i o
    , _replVarInfo :: VarInfo
    , _replResult :: EvalCompletion name o
    } deriving Generic

Lens.makeLenses ''Repl

instance Functor (Repl name i o) where
    fmap f = replExpr %~ hflipped %~ hmap (\_ -> Lens._Wrapped %~ f)

instance Foldable (Repl name i o) where
    foldMap f = (^. replExpr . hflipped . Lens.to (hfoldMap (\_ (Const x) -> f x)))

instance Traversable (Repl name i o) where
    traverse f = replExpr (htraverseFlipped (\_ -> Lens._Wrapped f))

data WorkArea name i o a = WorkArea
    { _waPanes :: [Pane name i o a]
    , _waRepl :: Repl name i o a
    , _waGlobals :: i [NameRef name o]
    } deriving (Functor, Foldable, Traversable, Generic)

Lens.makeLenses ''Definition
Lens.makeLenses ''DefinitionBuiltin
Lens.makeLenses ''Pane
Lens.makeLenses ''TagPane
Lens.makeLenses ''WorkArea
Lens.makePrisms ''DefinitionBody
Lens.makePrisms ''PaneBody
