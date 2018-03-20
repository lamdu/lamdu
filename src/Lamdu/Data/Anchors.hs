{-# LANGUAGE Rank2Types, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Lamdu.Data.Anchors
    ( Code(..), onCode
    , Revision(..), onRevision
    , Pane(..)
    , CodeAnchors, RevisionProps
    , assocBranchNameRef
    , assocTagNameRef
    , assocTag, anonTag
    , assocScopeRef
    , assocPresentationMode
    , assocDefinitionState
    , assocTagOrder
    , assocFieldParamList
    , BinderParamScopeId(..), bParamScopeId
    ) where

import qualified Control.Lens as Lens
import           Data.Binary (Binary)
import           Data.ByteString.Char8 ()
import           Data.UUID.Types (nil)
import           GUI.Momentu.State (GUIState)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Data.Meta (DefinitionState(..), SpecialArgs(..), PresentationMode, ParamList)
import           Lamdu.Eval.Results (ScopeId)
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Revision.Deltum.Rev.Branch (Branch)
import           Revision.Deltum.Rev.Version (Version)
import           Revision.Deltum.Rev.View (View)
import           Revision.Deltum.Transaction (MkProperty(..))
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

newtype Pane m = Pane
    { paneDef :: DefI m
    } deriving (Eq, Ord, Show, Generic)
instance Binary (Pane m)

data Code f m = Code
    { repl :: f (Definition.Expr (ValI m))
    , panes :: f [Pane m]
    , globals :: f (Set (DefI m))
    , preJumps :: f [WidgetId.Id]
    , preGuiState :: f GUIState
    , postGuiState :: f GUIState
    , tags :: f (Set T.Tag)
    , tids :: f (Set T.NominalId)
    }
onCode :: (forall a. Binary a => f a -> g a) -> Code f m -> Code g m
onCode f (Code x0 x1 x2 x3 x4 x5 x6 x7) =
    Code (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)

data Revision f m = Revision
    { branches :: f [Branch m]
    , currentBranch :: f (Branch m)
    , redos :: f [Version m]
    , view :: f (View m)
    }
onRevision :: (forall a. Binary a => f a -> g a) -> Revision f m -> Revision g m
onRevision f (Revision x0 x1 x2 x3) =
    Revision (f x0) (f x1) (f x2) (f x3)

newtype BinderParamScopeId = BinderParamScopeId
    { _bParamScopeId :: ScopeId
    } deriving (Eq, Ord, Binary)

type CodeAnchors m = Code (MkProperty m) m
type RevisionProps m = Revision (MkProperty m) m

assocTagNameRef :: Monad m => T.Tag -> MkProperty m Text
assocTagNameRef = Transaction.assocDataRefDef "" "Name" . UniqueId.toUUID

assocBranchNameRef :: Monad m => Branch m -> MkProperty m Text
assocBranchNameRef = Transaction.assocDataRefDef "" "Name" . UniqueId.toUUID

anonTag :: T.Tag
anonTag = UniqueId.identifierOfUUID nil & T.Tag

assocTag :: (UniqueId.ToUUID a, Monad m) => a -> MkProperty m T.Tag
assocTag = Transaction.assocDataRefDef anonTag "Tag" . UniqueId.toUUID

assocScopeRef :: Monad m => V.Var -> MkProperty m (Maybe BinderParamScopeId)
assocScopeRef = Transaction.assocDataRef "ScopeId" . UniqueId.toUUID

assocTagOrder :: Monad m => T.Tag -> MkProperty m Int
assocTagOrder = Transaction.assocDataRefDef 0 "Order" . UniqueId.toUUID

assocFieldParamList ::
    Monad m => ValI m -> Transaction.MkProperty m (Maybe ParamList)
assocFieldParamList = Transaction.assocDataRef "field param list" . UniqueId.toUUID

assocPresentationMode :: Monad m => V.Var -> Transaction.MkProperty m PresentationMode
assocPresentationMode =
    Transaction.assocDataRefDef Verbose "PresentationMode" . UniqueId.toUUID

assocDefinitionState :: Monad m => DefI m -> Transaction.MkProperty m DefinitionState
assocDefinitionState =
    Transaction.assocDataRefDef LiveDefinition "DefinitionState" . UniqueId.toUUID

Lens.makeLenses ''BinderParamScopeId
