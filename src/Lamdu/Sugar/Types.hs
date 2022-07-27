{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types
    ( module Exported
    , EntityId
    , PaneBody(..), _PaneDefinition
    , Pane(..), paneBody, paneClose, paneMoveDown, paneMoveUp, paneEntityId, paneDefinitionState
    , TagPane(..), tpTag, tpTagData, tpSetTexts, tpSetSymbol, tpSetOrder
    , WorkArea(..), waPanes, waGlobals, waOpenPane
    , Globals(..), globalDefs, globalNominals, globalTags
    , NameRef(..), nrName, nrId
    , GotoDest(..), _GoToDef, _GoToNom, _GoToTag
    , ParamKind(..), _TypeParam, _RowParam
    , NominalPane(..), npName, npParams, npNominalId, npEntityId, npBody
    , Definition(..), drName, drBody, drDefI, drGotoNextOutdated
    , DefinitionBody(..), _DefinitionBodyExpression, _DefinitionBodyBuiltin
    , DefinitionExpression(..), deContent, dePresentationMode, deType, deVarInfo, deResult
    , Meta.SpecialArgs(..), Meta.PresentationMode, Meta._Operator, Meta._Verbose
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
import           Lamdu.Sugar.Types.Lhs as Exported
import           Lamdu.Sugar.Types.Parts as Exported
import           Lamdu.Sugar.Types.Tag as Exported
import           Lamdu.Sugar.Types.TaggedList as Exported
import           Lamdu.Sugar.Types.Type as Exported

import           Lamdu.Prelude

data DefinitionExpression v name i o a = DefinitionExpression
    { _deType :: Scheme name Unit
    , _dePresentationMode :: Maybe (i (Property o Meta.PresentationMode))
    , _deContent :: Annotated a # Assignment v name i o
    , _deVarInfo :: VarInfo
    , _deResult :: EvalCompletion o
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
    { _drName :: OptionalTag name i o
    , _drDefI :: V.Var
    , _drBody :: DefinitionBody v name i o a
    , -- A temporary, inefficient and not ideal UX mechanism to iterate over outdated defs.
      -- Ideally the user should know ahead of time if there are any and how much.
      _drGotoNextOutdated :: o (Maybe EntityId)
    } deriving (Functor, Foldable, Traversable, Generic)

data TagPane o = TagPane
    { _tpTag :: T.Tag
    , _tpTagData :: DataTag.Tag
    , _tpSetSymbol :: DataTag.Symbol -> o ()
    , _tpSetTexts :: LangId -> DataTag.TextsInLang -> o ()
    , _tpSetOrder :: Int -> o ()
    } deriving Generic

data ParamKind = TypeParam | RowParam deriving (Eq, Ord, Generic)

data NominalPane name i o = NominalPane
    { _npName :: OptionalTag name i o
    , _npNominalId :: T.NominalId
    , _npEntityId :: EntityId
    , _npParams :: TaggedList name i o (Property o ParamKind)
    , _npBody :: Maybe (Scheme name o)
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
    , _paneClose :: o ()
    , _paneMoveDown :: Maybe (o ())
    , _paneMoveUp :: Maybe (o ())
    } deriving (Functor, Foldable, Traversable, Generic)

data NameRef name a = NameRef
    { _nrName :: name
    , _nrId :: a
    } deriving (Functor, Foldable, Traversable, Generic)

data Globals name i = Globals
    { _globalDefs     :: i [NameRef name V.Var]
    , _globalNominals :: i [NameRef name T.NominalId]
    , _globalTags     :: i [NameRef name T.Tag]
    } deriving Generic

data GotoDest = GoToDef V.Var | GoToNom T.NominalId | GoToTag T.Tag
    deriving Generic

data WorkArea v name i o a = WorkArea
    { _waPanes :: [Pane v name i o a]
    , _waGlobals :: Globals name i
    , _waOpenPane :: GotoDest -> o EntityId
    } deriving (Functor, Foldable, Traversable, Generic)

traverse Lens.makeLenses
    [''Definition, ''DefinitionBuiltin, ''Pane, ''NameRef, ''TagPane, ''Globals, ''WorkArea, ''NominalPane]
    <&> concat
traverse Lens.makePrisms [''DefinitionBody, ''PaneBody, ''ParamKind, ''GotoDest] <&> concat
