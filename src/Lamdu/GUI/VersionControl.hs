{-# LANGUAGE TemplateHaskell, RankNTypes, DerivingVia #-}
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
import qualified Lamdu.I18N.Texts as Texts
import qualified Lamdu.VersionControl.Actions as A
import           Revision.Deltum.Rev.Branch (Branch)
import qualified Revision.Deltum.Rev.Branch as Branch
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

branchNameFDConfig :: Texts.Versioning Text -> FocusDelegator.Config
branchNameFDConfig txt = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [MetaKey noMods MetaKey.Key'F2]
    , FocusDelegator.focusChildDoc =
        E.Doc [txt ^. Texts.branches, txt ^. Texts.rename]
    , FocusDelegator.focusParentKeys = [MetaKey noMods MetaKey.Key'Enter]
    , FocusDelegator.focusParentDoc =
        E.Doc [txt ^. Texts.branches, txt ^. Texts.doneRenaming]
    }

undoEventMap ::
    Texts.Versioning Text -> VersionControl.Config ->
    Maybe (m GuiState.Update) -> Gui EventMap m
undoEventMap txt config =
    E.keyPresses (config ^. VersionControl.undoKeys <&> toModKey)
    (E.Doc [txt ^. Texts.edit, txt ^. Texts.undo])
    & foldMap

redoEventMap ::
    Texts.Versioning Text -> VersionControl.Config ->
    Maybe (m GuiState.Update) -> Gui EventMap m
redoEventMap txt config =
    E.keyPresses (config ^. VersionControl.redoKeys <&> toModKey)
    (E.Doc [txt ^. Texts.edit, txt ^. Texts.redo])
    & foldMap

eventMap ::
    (MonadReader env m, Applicative f, Texts.HasLanguage env) =>
    m (VersionControl.Config -> A.Actions t f -> Gui EventMap f)
eventMap =
    Lens.view (Texts.texts . Texts.versioning)
    <&> \txt config actions ->
    mconcat
    [ E.keysEventMapMovesCursor (config ^. VersionControl.makeBranchKeys)
      (E.Doc [txt ^. Texts.branches, txt ^. Texts.new]) $ branchTextEditId <$> A.makeBranch actions
    , E.keysEventMapMovesCursor (config ^. VersionControl.jumpToBranchesKeys)
      (E.Doc [txt ^. Texts.branches, txt ^. Texts.select]) $
      (pure . branchDelegatorId . Property.value . A.currentBranch) actions
    , A.mUndo actions <&> fmap GuiState.fullUpdate & undoEventMap txt config
    , A.mRedo actions <&> fmap GuiState.fullUpdate & redoEventMap txt config
    ]

branchDelegatorId :: Branch t -> Widget.Id
branchDelegatorId = WidgetIds.fromUUID . Branch.uuid

branchTextEditId :: Branch t -> Widget.Id
branchTextEditId = (`Widget.joinId` ["textedit"]) . branchDelegatorId

makeBranchSelector ::
    ( MonadReader env mr, Monad n, GuiState.HasCursor env, TextEdit.HasStyle env
    , Applicative mw, Hover.HasStyle env, Element.HasAnimIdPrefix env
    , VersionControl.HasConfig env, VersionControl.HasTheme env
    , Texts.HasLanguage env
    ) =>
    (forall a. Transaction n a -> mw a) ->
    (forall a. Transaction n a -> mr a) ->
    A.Actions n mw -> mr (TextWidget mw)
makeBranchSelector rwtransaction rtransaction actions =
    do
        txt <- Lens.view (Texts.texts . Texts.versioning)
        let makeBranchNameEdit branch =
                do
                    nameProp <-
                        Anchors.assocBranchNameRef branch ^. Property.mkProperty
                        <&> Property.pSet . Lens.mapped %~ rwtransaction
                        & rtransaction
                    branchNameEdit <-
                        (FocusDelegator.make ?? branchNameFDConfig txt
                         ?? FocusDelegator.FocusEntryParent ?? branchDelegatorId branch
                        ) <*>
                        ( TextEdits.makeLineEdit ?? empty ?? nameProp ?? branchTextEditId branch
                          <&> (^. Align.tValue) )
                    config <- Lens.view VersionControl.config
                    let delEventMap
                            | List.isLengthAtLeast 2 (A.branches actions) =
                                E.keysEventMapMovesCursor
                                (config ^. VersionControl.delBranchKeys)
                                (E.Doc
                                    [ txt ^. Texts.branches
                                    , txt ^. Texts.delete
                                    ])
                                (branchDelegatorId <$> A.deleteBranch actions branch)
                            | otherwise = mempty
                    pure (branch, Widget.weakerEvents delEventMap branchNameEdit)
                    & if branch == Property.value (A.currentBranch actions)
                        then
                            Reader.local $
                            \env ->
                            env &
                            TextView.color .~
                            env ^. VersionControl.theme . VersionControl.selectedBranchColor
                        else id
        branchNameEdits <- A.branches actions & traverse makeBranchNameEdit
        Choice.make ?? A.currentBranch actions
            ?? branchNameEdits
            ?? Choice.defaultConfig (txt ^. Texts.branches)
            ?? WidgetIds.branchSelection
            <&> WithTextPos 0 -- TODO: Should come from Choice
    where
        empty =
            TextEdit.Modes
            { TextEdit._unfocused = "(?)"
            , TextEdit._focused = ""
            }
