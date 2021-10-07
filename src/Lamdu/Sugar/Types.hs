{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types
    ( module Exported
    , EntityId
    , PaneBody(..), _PaneDefinition
    , Pane(..), paneBody, paneClose, paneMoveDown, paneMoveUp, paneEntityId, paneDefinitionState
    , TagPane(..), tpTag, tpTagData, tpSetTexts, tpSetSymbol, tpSetOrder
    , Repl(..), replExpr, replVarInfo, replResult
    , WorkArea(..), waPanes, waRepl, waGlobals
    , Globals(..), allGlobals, globalDefs, globalNominals, globalTags
    , NominalTypeBody(..), nominalType, nominalParams
    , NominalPaneBody(..), _NominalPaneOpaque, _NominalPaneType
    , NominalPane(..), npName, npNominalId, npEntityId, npBody
    , Definition(..), drName, drBody, drDefI
    , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
    , DefinitionExpression(..), deContent, dePresentationMode, deType
    , Meta.SpecialArgs(..), Meta.PresentationMode
    , Meta.DefinitionState(..)
    , DefinitionBuiltin(..), biType, biName, biSetName
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit)
import           Data.Property (Property)
import           Hyper
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Meta as Meta
import qualified Lamdu.Data.Tag as DataTag
import           Lamdu.I18N.LangId (LangId(..))
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval as Exported
import           Lamdu.Sugar.Types.Expression as Exported
import           Lamdu.Sugar.Types.GetVar as Exported
import           Lamdu.Sugar.Types.NameRef as Exported
import           Lamdu.Sugar.Types.Parts as Exported
import           Lamdu.Sugar.Types.Tag as Exported
import           Lamdu.Sugar.Types.Type as Exported

import           Lamdu.Prelude

data DefinitionExpression v name i o a = DefinitionExpression
    { _deType :: Scheme name Unit
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
    , _biType :: Scheme name Unit
    } deriving Generic

data DefinitionBody v name i o a
    = DefinitionBodyExpression (DefinitionExpression v name i o a)
    | DefinitionBodyBuiltin (DefinitionBuiltin name o)
    deriving (Functor, Foldable, Traversable, Generic)

data Definition v name i o a = Definition
    { _drName :: TagRef name i o
    , _drDefI :: V.Var
    , _drBody :: DefinitionBody v name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data TagPane o = TagPane
    { _tpTag :: T.Tag
    , _tpTagData :: DataTag.Tag
    , _tpSetSymbol :: DataTag.Symbol -> o ()
    , _tpSetTexts :: LangId -> DataTag.TextsInLang -> o ()
    , _tpSetOrder :: Int -> o ()
    } deriving Generic

data NominalTypeBody name o = NominalTypeBody
    { _nominalType :: Scheme name o
    , _nominalParams :: () -- TODO: (what is sugared 'NomVarTypes typ # QVars')?
    } deriving Generic

data NominalPaneBody name o
    = NominalPaneOpaque
    | NominalPaneType (NominalTypeBody name o)
    deriving Generic

data NominalPane name i o = NominalPane
    { _npName :: TagRef name i o
    , _npNominalId :: T.NominalId
    , _npEntityId :: EntityId
    , _npBody :: NominalPaneBody name o
    } deriving Generic

data PaneBody v name i o a
    = PaneDefinition (Definition v name i o a)
    | PaneTag (TagPane o)
    | PaneNominal (NominalPane name i o)
    deriving (Functor, Foldable, Traversable, Generic)

data Pane v name i o a = Pane
    { _paneBody :: PaneBody v name i o a
    , _paneEntityId :: EntityId
    , _paneDefinitionState :: Property o Meta.DefinitionState
    , _paneClose :: o EntityId
    , _paneMoveDown :: Maybe (o ())
    , _paneMoveUp :: Maybe (o ())
    } deriving (Functor, Foldable, Traversable, Generic)

data Repl v name i o a = Repl
    { _replExpr :: Annotated a # Binder v name i o
    , _replVarInfo :: VarInfo
    , _replResult :: EvalCompletion o
    } deriving Generic

Lens.makeLenses ''Repl

instance Functor (Repl v name i o) where
    fmap f = replExpr %~ hflipped %~ hmap (\_ -> Lens._Wrapped %~ f)

instance Foldable (Repl v name i o) where
    foldMap f = (^. replExpr . hflipped . Lens.to (hfoldMap (\_ (Const x) -> f x)))

instance Traversable (Repl v name i o) where
    traverse f = replExpr (htraverseFlipped (\_ -> Lens._Wrapped f))

data Globals name i o = Globals
    { _globalDefs     :: i [NameRef name o]
    , _globalNominals :: i [NameRef name o]
    , _globalTags     :: i [NameRef name o]
    } deriving Generic

-- In the future, maybe the different NameRefs will have different
-- types, but for now they're traversable
allGlobals :: Lens.Traversal (Globals name i o) (Globals name' i' o') (i [NameRef name o]) (i' [NameRef name' o'])
allGlobals f (Globals x y z) = Globals <$> f x <*> f y <*> f z

data WorkArea v name i o a = WorkArea
    { _waPanes :: [Pane v name i o a]
    , _waRepl :: Repl v name i o a
    , _waGlobals :: Globals name i o
    } deriving (Functor, Foldable, Traversable, Generic)

traverse Lens.makeLenses
    [''Definition, ''DefinitionBuiltin, ''Pane, ''TagPane, ''Globals, ''WorkArea
    , ''NominalPane, ''NominalTypeBody]
    <&> concat
traverse Lens.makePrisms [''NominalPaneBody, ''DefinitionBody, ''PaneBody] <&> concat
