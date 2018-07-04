{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.VersionControl
    ( makeBranchSelector, eventMap
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.List.Extended as List
import qualified Data.Property as Property
import           GUI.Momentu.Align (WithTextPos(..), TextWidget)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.MetaKey (MetaKey(..), noMods, toModKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
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
    Gui EventMap m
undoEventMap config =
    E.keyPresses (config ^. VersionControl.undoKeys <&> toModKey)
    (E.Doc ["Edit", "Undo"])
    & foldMap

redoEventMap ::
    VersionControl.Config -> Maybe (m GuiState.Update) ->
    Gui EventMap m
redoEventMap config =
    E.keyPresses (config ^. VersionControl.redoKeys <&> toModKey)
    (E.Doc ["Edit", "Redo"])
    & foldMap

eventMap ::
    Applicative f =>
    VersionControl.Config -> Actions t f ->
    Gui EventMap f
eventMap config actions = mconcat
    [ E.keysEventMapMovesCursor (config ^. VersionControl.makeBranchKeys)
      (E.Doc ["Branches", "New"]) $ branchTextEditId <$> makeBranch actions
    , E.keysEventMapMovesCursor (config ^. VersionControl.jumpToBranchesKeys)
      (E.Doc ["Branches", "Select"]) $
      (pure . branchDelegatorId . Property.value . currentBranch) actions
    , mUndo actions <&> fmap GuiState.fullUpdate & undoEventMap config
    , mRedo actions <&> fmap GuiState.fullUpdate & redoEventMap config
    ]

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
    Actions n mw -> mr (TextWidget mw)
makeBranchSelector rwtransaction rtransaction actions =
    do
        branchNameEdits <- branches actions & traverse makeBranchNameEdit
        Choice.make ?? currentBranch actions
            ?? branchNameEdits
            ?? Choice.defaultConfig "Branches"
            ?? WidgetIds.branchSelection
            <&> WithTextPos 0 -- TODO: Should come from Choice
    where
        empty =
            TextEdit.Modes
            { TextEdit._unfocused = "unnamed branch"
            , TextEdit._focused = ""
            }
        makeBranchNameEdit branch =
            do
                nameProp <-
                    Anchors.assocBranchNameRef branch ^. Property.mkProperty
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
                        | List.isLengthAtLeast 2 (branches actions) =
                            E.keysEventMapMovesCursor
                            (config ^. VersionControl.delBranchKeys)
                            (E.Doc ["Branches", "Delete"])
                            (branchDelegatorId <$> deleteBranch actions branch)
                        | otherwise = mempty
                pure (branch, Widget.weakerEvents delEventMap branchNameEdit)
                & if branch == Property.value (currentBranch actions)
                    then
                        Reader.local $
                        \env ->
                        env &
                        TextView.color .~
                        env ^. VersionControl.theme . VersionControl.selectedBranchColor
                    else id
