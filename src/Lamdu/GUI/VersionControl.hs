{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.VersionControl
    ( makeBranchSelector, eventMap
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.List.Utils as ListUtils
import qualified Data.Property as Property
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods, toModKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Choice as Choice
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.VersionControl.Actions (Actions(..))
import           Revision.Deltum.Rev.Branch (Branch)
import qualified Revision.Deltum.Rev.Branch as Branch
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

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
    & foldMap

redoEventMap ::
    VersionControl.Config -> Maybe (m GuiState.Update) ->
    EventMap (m GuiState.Update)
redoEventMap config =
    E.keyPresses (VersionControl.redoKeys config <&> toModKey) (E.Doc ["Edit", "Redo"])
    & foldMap

eventMap ::
    Applicative f =>
    VersionControl.Config -> Actions t f ->
    EventMap (f GuiState.Update)
eventMap config actions = mconcat
    [ E.keysEventMapMovesCursor (VersionControl.makeBranchKeys config)
      (E.Doc ["Branches", "New"]) $ branchTextEditId <$> makeBranch actions
    , E.keysEventMapMovesCursor (VersionControl.jumpToBranchesKeys config)
      (E.Doc ["Branches", "Select"]) $
      (pure . branchDelegatorId . currentBranch) actions
    , mUndo actions <&> fmap GuiState.fullUpdate & undoEventMap config
    , mRedo actions <&> fmap GuiState.fullUpdate & redoEventMap config
    ]

choiceWidgetConfig :: Choice.Config
choiceWidgetConfig =
    Choice.Config
    { Choice.cwcFDConfig = Choice.defaultFdConfig "Branches"
    , Choice.cwcExpandMode = Choice.ExplicitEntry
    , Choice.cwcOrientation = Choice.Vertical
    }

branchDelegatorId :: Branch t -> Widget.Id
branchDelegatorId = WidgetIds.fromUUID . Branch.uuid

branchTextEditId :: Branch t -> Widget.Id
branchTextEditId = (`Widget.joinId` ["textedit"]) . branchDelegatorId

makeBranchSelector ::
    ( MonadReader env mr, GuiState.HasCursor env, TextEdit.HasStyle env
    , Applicative mw, Hover.HasStyle env, Element.HasAnimIdPrefix env
    , VersionControl.HasConfig env, VersionControl.HasTheme env
    , Monad n
    ) =>
    (forall a. Transaction n a -> mw a) ->
    (forall a. Transaction n a -> mr a) ->
    Actions n mw ->
    mr (Widget (mw GuiState.Update))
makeBranchSelector rwtransaction rtransaction actions =
    do
        branchNameEdits <- branches actions & traverse makeBranchNameEdit
        Choice.make ?? setCurrentBranch actions
            ?? branchNameEdits ?? currentBranch actions
            ?? choiceWidgetConfig
            ?? WidgetIds.branchSelection
    where
        empty = TextEdit.EmptyStrings "unnamed branch" ""
        makeBranchNameEdit branch =
            do
                nameProp <-
                    Anchors.assocBranchNameRef branch ^. Transaction.mkProperty
                    <&> Property.pSet . Lens.mapped %~ rwtransaction
                    & rtransaction
                branchNameEdit <-
                    (FocusDelegator.make ?? branchNameFDConfig
                     ?? FocusDelegator.FocusEntryParent ?? branchDelegatorId branch
                    ) <*>
                    ( TextEdits.makeLineEdit ?? empty ?? nameProp ?? branchTextEditId branch
                      <&> (^. Align.tValue) )
                config <- Lens.view VersionControl.config
                let delEventMap
                        | ListUtils.isLengthAtLeast 2 (branches actions) =
                            E.keysEventMapMovesCursor
                            (VersionControl.delBranchKeys config)
                            (E.Doc ["Branches", "Delete"])
                            (branchDelegatorId <$> deleteBranch actions branch)
                        | otherwise = mempty
                pure (branch, Widget.weakerEvents delEventMap branchNameEdit)
                & if branch == currentBranch actions
                    then
                        Reader.local $
                        \env ->
                        env &
                        TextView.color .~
                        VersionControl.selectedBranchColor (env ^. VersionControl.theme)
                    else id
