{-# LANGUAGE Rank2Types, TemplateHaskell, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Lamdu.Data.Anchors
    ( Gui(..), onGui
    , Code(..), onCode
    , Revision(..), onRevision
    , Pane(..), _PaneDefinition
    , GuiAnchors, CodeAnchors, RevisionProps
    , HasCodeAnchors(..)
    , assocBranchNameRef
    , assocTag, anonTag
    , assocScopeRef
    , assocPresentationMode
    , assocDefinitionState
    , BinderParamScopeId(..), bParamScopeId
    ) where

import qualified Control.Lens as Lens
import           Data.ByteString.Char8 ()
import           Data.Property (MkProperty, MkProperty')
import           Data.UUID.Types (nil)
import           GUI.Momentu.State (GUIState)
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Meta (DefinitionState(..), SpecialArgs(..), PresentationMode)
import           Lamdu.Eval.Results (ScopeId)
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Revision.Deltum.Rev.Branch (Branch)
import           Revision.Deltum.Rev.Version (Version)
import           Revision.Deltum.Rev.View (View)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Pane m
    = PaneDefinition (DefI m)
    | PaneNominal T.NominalId
    | PaneTag T.Tag
    deriving (Eq, Show, Generic)
    deriving anyclass Binary

data Gui f = Gui
    { preJumps :: f [WidgetId.Id]
    , preGuiState :: f GUIState
    , postGuiState :: f GUIState
    }
onGui :: (forall a. Binary a => f a -> g a) -> Gui f -> Gui g
onGui f (Gui x0 x1 x2) = Gui (f x0) (f x1) (f x2)

data Code f m = Code
    { panes :: f [Pane m]
    , globals :: f (Set (DefI m))
    , tags :: f (Set T.Tag)
    , tids :: f (Set T.NominalId)
    }
onCode :: (forall a. Binary a => f a -> g a) -> Code f m -> Code g m
onCode f (Code x0 x1 x2 x3) = Code (f x0) (f x1) (f x2) (f x3)

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
    }
    deriving stock (Generic, Show)
    deriving newtype (Eq, Ord, Binary)

type GuiAnchors i o = Gui (MkProperty i o)
type CodeAnchors m = Code (MkProperty' (T m)) m
type RevisionProps m = Revision (MkProperty' (T m)) m

class HasCodeAnchors env m where
    codeAnchors :: Lens' env (CodeAnchors m)

instance HasCodeAnchors (Code (MkProperty (T m) (T m)) m) m where codeAnchors = id

assocBranchNameRef :: Monad m => Branch m -> MkProperty' (T m) Text
assocBranchNameRef = Transaction.assocDataRefDef "" "Name" . UniqueId.toUUID

anonTag :: T.Tag
anonTag = UniqueId.identifierOfUUID nil & T.Tag

assocTag :: (UniqueId.ToUUID a, Monad m) => a -> MkProperty' (T m) T.Tag
assocTag = Transaction.assocDataRefDef anonTag "Tag" . UniqueId.toUUID

assocScopeRef :: Monad m => V.Var -> MkProperty' (T m) (Maybe BinderParamScopeId)
assocScopeRef = Transaction.assocDataRef "ScopeId" . UniqueId.toUUID

assocPresentationMode :: Monad m => V.Var -> MkProperty' (T m) PresentationMode
assocPresentationMode =
    Transaction.assocDataRefDef Verbose "PresentationMode" . UniqueId.toUUID

assocDefinitionState :: (Monad m, UniqueId.ToUUID a) => a -> MkProperty' (T m) DefinitionState
assocDefinitionState =
    Transaction.assocDataRefDef LiveDefinition "DefinitionState" . UniqueId.toUUID

Lens.makeLenses ''BinderParamScopeId
Lens.makePrisms ''Pane
