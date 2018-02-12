{-# LANGUAGE NoImplicitPrelude, TypeOperators, OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.VersionControl
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.List.Utils as ListUtils
import qualified Data.Store.Property as Property
import           Data.Store.Rev.Branch (Branch)
import qualified Data.Store.Rev.Branch as Branch
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey(..), noMods, toModKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.VersionControl.Actions (Actions(..))

import           Lamdu.Prelude

branchNameFDConfig :: FocusDelegator.Config
branchNameFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'F2]
    , FocusDelegator.focusChildDoc = E.Doc ["Branches", "Rename"]
    , FocusDelegator.focusParentKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusParentDoc = E.Doc ["Branches", "Done renaming"]
    }

undoEventMap ::
    VersionControl.Config -> Maybe (m GuiState.Update) ->
    EventMap (m GuiState.Update)
undoEventMap config =
    E.keyPresses (VersionControl.undoKeys config <&> toModKey) (E.Doc ["Edit", "Undo"])
    & maybe mempty

redoEventMap ::
    VersionControl.Config -> Maybe (m GuiState.Update) ->
    EventMap (m GuiState.Update)
redoEventMap config =
    E.keyPresses (VersionControl.redoKeys config <&> toModKey) (E.Doc ["Edit", "Redo"])
    & maybe mempty

globalEventMap ::
    Applicative f =>
    VersionControl.Config -> Actions t f ->
    EventMap (f GuiState.Update)
globalEventMap config actions = mconcat
    [ E.keysEventMapMovesCursor (VersionControl.makeBranchKeys config)
      (E.Doc ["Branches", "New"]) $ branchTextEditId <$> makeBranch actions
    , E.keysEventMapMovesCursor (VersionControl.jumpToBranchesKeys config)
      (E.Doc ["Branches", "Select"]) $
      (pure . branchDelegatorId . currentBranch) actions
    , mUndo actions <&> fmap GuiState.fullUpdate & undoEventMap config
    , mRedo actions <&> fmap GuiState.fullUpdate & redoEventMap config
    ]

choiceWidgetConfig :: VersionControl.Theme -> Choice.Config
choiceWidgetConfig theme =
    Choice.Config
    { Choice.cwcFDConfig =
        FocusDelegator.Config
        { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'Enter]
        , FocusDelegator.focusChildDoc = E.Doc ["Branches", "Select"]
        , FocusDelegator.focusParentKeys = [MetaKey noMods MetaKey.Key'Enter]
        , FocusDelegator.focusParentDoc = E.Doc ["Branches", "Choose selected"]
        }
    , Choice.cwcExpandMode = VersionControl.selectedBranchColor theme & Choice.AutoExpand
    , Choice.cwcOrientation = Choice.Vertical
    }

branchDelegatorId :: Branch t -> Widget.Id
branchDelegatorId = WidgetIds.fromUUID . Branch.uuid

branchTextEditId :: Branch t -> Widget.Id
branchTextEditId = (`Widget.joinId` ["textedit"]) . branchDelegatorId

make ::
    (MonadReader env mr, GuiState.HasCursor env, TextEdit.HasStyle env,
     Applicative mw, Monad n) =>
    VersionControl.Config -> VersionControl.Theme ->
    (forall a. Transaction n a -> mw a) ->
    (forall a. Transaction n a -> mr a) ->
    Actions n mw ->
    (Widget (mw GuiState.Update) -> mr (Widget (mw GuiState.Update))) ->
    mr (Widget (mw GuiState.Update))
make config theme rwtransaction rtransaction actions mkWidget =
    do
        branchNameEdits <- branches actions & traverse makeBranchNameEdit
        branchSelector <-
            Choice.make ?? setCurrentBranch actions
            ?? branchNameEdits ?? currentBranch actions
            ?? choiceWidgetConfig theme
            ?? WidgetIds.branchSelection
        mkWidget branchSelector
            <&> Widget.eventMapMaker . Lens.mapped %~ (globalEventMap config actions <>)
    where
        empty = TextEdit.EmptyStrings "unnamed branch" ""
        makeBranchNameEdit branch =
            do
                nameProp <-
                    Anchors.assocNameRef (Branch.uuid branch) ^. Transaction.mkProperty
                    <&> Property.pSet . Lens.mapped %~ rwtransaction
                    & rtransaction
                branchNameEdit <-
                    (FocusDelegator.make ?? branchNameFDConfig
                     ?? FocusDelegator.FocusEntryParent ?? branchDelegatorId branch
                    ) <*>
                    ( TextEdits.makeLineEdit ?? empty ?? nameProp ?? branchTextEditId branch
                      <&> (^. Align.tValue) )
                let delEventMap
                        | ListUtils.isLengthAtLeast 2 (branches actions) =
                            E.keysEventMapMovesCursor
                            (VersionControl.delBranchKeys config)
                            (E.Doc ["Branches", "Delete"])
                            (branchDelegatorId <$> deleteBranch actions branch)
                        | otherwise = mempty
                pure (branch, Widget.weakerEvents delEventMap branchNameEdit)
