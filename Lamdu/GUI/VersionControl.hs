{-# LANGUAGE NoImplicitPrelude ,RecordWildCards, TypeOperators, OverloadedStrings, RankNTypes #-}
module Lamdu.GUI.VersionControl
    ( make
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (lift)
import qualified Data.List.Utils as ListUtils
import qualified Data.Store.Property as Property
import           Data.Store.Rev.Branch (Branch)
import qualified Data.Store.Rev.Branch as Branch
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFW as GLFW
import qualified Lamdu.Data.Anchors as Anchors
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Lamdu.GUI.VersionControl.Config as VersionControl
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.VersionControl.Actions (Actions(..))

branchNameFDConfig :: FocusDelegator.Config
branchNameFDConfig = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'F2]
    , FocusDelegator.focusChildDoc = E.Doc ["Branches", "Rename"]
    , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Enter]
    , FocusDelegator.focusParentDoc = E.Doc ["Branches", "Done renaming"]
    }

undoEventMap ::
    Functor m =>
    VersionControl.Config -> Maybe (m Widget.Id) ->
    Widget.EventMap (m Widget.EventResult)
undoEventMap VersionControl.Config{..} =
    maybe mempty .
    Widget.keysEventMapMovesCursor undoKeys $ E.Doc ["Edit", "Undo"]

redoEventMap ::
    Functor m =>
    VersionControl.Config -> Maybe (m Widget.Id) ->
    Widget.EventMap (m Widget.EventResult)
redoEventMap VersionControl.Config{..} =
    maybe mempty .
    Widget.keysEventMapMovesCursor redoKeys $ E.Doc ["Edit", "Redo"]

globalEventMap ::
    Applicative f =>
    VersionControl.Config -> Actions t f ->
    Widget.EventMap (f Widget.EventResult)
globalEventMap VersionControl.Config{..} actions = mconcat
    [ Widget.keysEventMapMovesCursor makeBranchKeys
      (E.Doc ["Branches", "New"]) $ branchTextEditId <$> makeBranch actions
    , Widget.keysEventMapMovesCursor jumpToBranchesKeys
      (E.Doc ["Branches", "Select"]) $
      (pure . branchDelegatorId . currentBranch) actions
    , undoEventMap VersionControl.Config{..} $ mUndo actions
    , redoEventMap VersionControl.Config{..} $ mRedo actions
    ]

choiceWidgetConfig :: VersionControl.Config -> Anim.Layer -> Choice.Config
choiceWidgetConfig VersionControl.Config{..} choiceBGLayer = Choice.Config
    { Choice.cwcFDConfig =
        FocusDelegator.Config
        { FocusDelegator.focusChildKeys = [ModKey mempty GLFW.Key'Enter]
        , FocusDelegator.focusChildDoc = E.Doc ["Branches", "Select"]
        , FocusDelegator.focusParentKeys = [ModKey mempty GLFW.Key'Enter]
        , FocusDelegator.focusParentDoc = E.Doc ["Branches", "Choose selected"]
        }
    , Choice.cwcExpandMode = Choice.AutoExpand selectedBranchColor
    , Choice.cwcOrientation = Box.vertical
    , Choice.cwcBgLayer = choiceBGLayer
    }

branchDelegatorId :: Branch t -> Widget.Id
branchDelegatorId = WidgetIds.fromUUID . Branch.uuid

branchTextEditId :: Branch t -> Widget.Id
branchTextEditId = (`Widget.joinId` ["textedit"]) . branchDelegatorId

make ::
    (Monad mr, Applicative mw, Monad n) =>
    VersionControl.Config -> Anim.Layer ->
    (forall a. Transaction n a -> mw a) ->
    (forall a. Transaction n a -> mr a) ->
    Actions n mw ->
    (Widget (mw Widget.EventResult) -> WidgetEnvT mr (Widget (mw Widget.EventResult))) ->
    WidgetEnvT mr (Widget (mw Widget.EventResult))
make VersionControl.Config{..} choiceBGLayer rwtransaction rtransaction actions mkWidget =
    do
        branchNameEdits <- traverse makeBranchNameEdit $ branches actions
        branchSelector <-
            BWidgets.makeChoiceWidget (setCurrentBranch actions)
            branchNameEdits (currentBranch actions)
            (choiceWidgetConfig VersionControl.Config{..} choiceBGLayer)
            WidgetIds.branchSelection
        mkWidget branchSelector
            <&> Widget.strongerEvents
                (globalEventMap VersionControl.Config{..} actions)
    where
        makeBranchNameEdit branch =
            do
                nameProp <-
                    Anchors.assocNameRef (Branch.uuid branch) ^. Transaction.mkProperty
                    & Lens.mapped . Property.pSet . Lens.mapped %~ rwtransaction
                    & rtransaction & lift
                branchNameEdit <-
                    BWidgets.makeLineEdit nameProp (branchTextEditId branch)
                    >>= BWidgets.makeFocusDelegator branchNameFDConfig
                        FocusDelegator.FocusEntryParent (branchDelegatorId branch)
                let delEventMap
                        | ListUtils.isLengthAtLeast 2 (branches actions) =
                            Widget.keysEventMapMovesCursor
                            delBranchKeys (E.Doc ["Branches", "Delete"])
                            (branchDelegatorId <$> deleteBranch actions branch)
                        | otherwise = mempty
                return (branch, branchNameEdit & Widget.weakerEvents delEventMap)
