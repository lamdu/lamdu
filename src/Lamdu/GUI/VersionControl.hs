{-# LANGUAGE RankNTypes, DerivingVia #-}
module Lamdu.GUI.VersionControl
    ( makeBranchSelector, eventMap
    ) where

import qualified Control.Lens as Lens
import qualified Data.List.Extended as List
import qualified Data.Property as Property
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (ModKey, noMods)
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.DropDownList as DropDownList
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextEdit.Property as TextEdits
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Versioning as Texts
import qualified Lamdu.VersionControl.Actions as A
import           Revision.Deltum.Rev.Branch (Branch)
import qualified Revision.Deltum.Rev.Branch as Branch
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

branchNameFDConfig :: _ => env -> FocusDelegator.Config
branchNameFDConfig txt = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [noMods ModKey.Key'F2]
    , FocusDelegator.focusChildDoc =
        E.toDoc txt [has . Texts.branches, has . Texts.rename]
    , FocusDelegator.focusParentKeys = [noMods ModKey.Key'Enter]
    , FocusDelegator.focusParentDoc =
        E.toDoc txt [has . Texts.branches, has . Texts.doneRenaming]
    }

undoEventMap ::
    _ => env -> VersionControl.Config ModKey ->
    Maybe (m GuiState.Update) -> EventMap (m GuiState.Update)
undoEventMap env config =
    E.keyPresses (config ^. VersionControl.undoKeys)
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.undo])
    & foldMap

redoEventMap ::
    _ =>
    env -> VersionControl.Config ModKey -> Maybe (m GuiState.Update) ->
    EventMap (m GuiState.Update)
redoEventMap env config =
    E.keyPresses (config ^. VersionControl.redoKeys)
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.redo])
    & foldMap

eventMap ::
    _ => m (VersionControl.Config ModKey -> A.Actions t f -> EventMap (f GuiState.Update))
eventMap =
    Lens.view id
    <&> \env config actions ->
    let toDoc = E.toDoc env in
    mconcat
    [ A.makeBranch actions
        <&> branchTextEditId
        & E.keysEventMapMovesCursor (config ^. VersionControl.makeBranchKeys)
        (toDoc [has . Texts.branches, has . Texts.new])
    , A.currentBranch actions & Property.value & branchDelegatorId & pure
        & E.keysEventMapMovesCursor
        (config ^. VersionControl.jumpToBranchesKeys)
        (toDoc [has . Texts.branches, has . MomentuTexts.choose])
    , A.mUndo actions <&> fmap GuiState.fullUpdate & undoEventMap env config
    , A.mRedo actions <&> fmap GuiState.fullUpdate & redoEventMap env config
    ]

branchDelegatorId :: Branch t -> Widget.Id
branchDelegatorId = WidgetIds.fromUUID . Branch.uuid

branchTextEditId :: Branch t -> Widget.Id
branchTextEditId = (`Widget.joinId` ["textedit"]) . branchDelegatorId

makeBranchSelector ::
    _ =>
    (forall a. Transaction n a -> mw a) ->
    (forall a. Transaction n a -> mr a) ->
    A.Actions n mw -> mr (TextWidget mw)
makeBranchSelector rwtransaction rtransaction actions =
    do
        txt <- Lens.view id
        let makeBranchNameEdit branch =
                do
                    nameProp <-
                        Anchors.assocBranchNameRef branch ^. Property.mkProperty
                        <&> Property.pSet . Lens.mapped %~ rwtransaction
                        & rtransaction
                    branchNameEdit <-
                        ( FocusDelegator.make ?? branchNameFDConfig txt
                        ?? FocusDelegator.FocusEntryParent
                        ?? branchDelegatorId branch
                        <&> (Align.tValue %~) )
                        <*> (TextEdits.makeLineEdit ?? empty ?? nameProp
                                ?? branchTextEditId branch)
                    config <- Lens.view has
                    let delEventMap
                            | List.isLengthAtLeast 2 (A.branches actions) =
                                E.keysEventMapMovesCursor
                                (config ^. VersionControl.delBranchKeys)
                                (E.toDoc txt
                                    [ has . Texts.branches
                                    , has . MomentuTexts.delete
                                    ])
                                (branchDelegatorId <$> A.deleteBranch actions branch)
                            | otherwise = mempty
                    pure
                        ( branch
                        , branchNameEdit
                            & Align.tValue %~ Widget.weakerEvents delEventMap
                        )
                    & if branch == Property.value (A.currentBranch actions)
                        then
                            local $
                            \env ->
                            env &
                            TextView.color .~
                            env ^. has . VersionControl.selectedBranchColor
                        else id
        branchNameEdits <- A.branches actions & traverse makeBranchNameEdit
        defConfig <- DropDownList.defaultConfig ?? txt ^. has . Texts.branches
        DropDownList.make ?? A.currentBranch actions ?? branchNameEdits
            ?? defConfig ?? WidgetIds.branchSelection
    where
        empty =
            TextEdit.Modes
            { TextEdit._unfocused = "(?)"
            , TextEdit._focused = ""
            }
