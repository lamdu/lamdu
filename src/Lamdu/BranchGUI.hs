{-# LANGUAGE TypeOperators, OverloadedStrings, RankNTypes #-}
module Lamdu.BranchGUI
  ( make
  , branchNameProp
  ) where

import Control.Applicative (Applicative, (<$>), pure)
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.VersionControl.Actions (Actions(..))
import Lamdu.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Data.List.Utils as ListUtils
import qualified Data.Store.Property as Property
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Edges as Edges
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

branchNameFDConfig :: FocusDelegator.Config
branchNameFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyF2]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Branches", "Rename"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Branches", "Done renaming"]
  }

branchSelectionFocusDelegatorConfig :: FocusDelegator.Config
branchSelectionFocusDelegatorConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Branches", "Select"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Branches", "Choose selected"]
  }

undoEventMap :: Functor m => Maybe (m Widget.Id) -> Widget.EventHandlers m
undoEventMap =
  maybe mempty (Widget.keysEventMapMovesCursor Config.undoKeys (E.Doc ["Edit", "Undo"]))

redoEventMap :: Functor m => Maybe (m Widget.Id) -> Widget.EventHandlers m
redoEventMap =
  maybe mempty (Widget.keysEventMapMovesCursor Config.redoKeys (E.Doc ["Edit", "Redo"]))

branchNameProp ::
  MonadA m => Branch (Tag m) -> Transaction.MkProperty m String
branchNameProp = Transaction.assocDataRefDef "" "name" . Branch.guid

globalEventMap :: Applicative f => Actions t f -> Widget.EventHandlers f
globalEventMap actions = mconcat
  [ Widget.keysEventMapMovesCursor Config.makeBranchKeys (E.Doc ["Branches", "New"]) .
    fmap
    (FocusDelegator.delegatingId .
     WidgetIds.fromGuid . Branch.guid) $ makeBranch actions
  , Widget.keysEventMapMovesCursor Config.jumpToBranchesKeys
    (E.Doc ["Branches", "Select"]) $ pure currentBranchWidgetId
  , undoEventMap $ mUndo actions
  , redoEventMap $ mRedo actions
  ]
  where
    currentBranchWidgetId = WidgetIds.fromGuid . Branch.guid $ currentBranch actions

make ::
  (MonadA m, MonadA n) =>
  (forall a. Transaction n a -> m a) ->
  Widget.Size -> Actions (Tag n) m -> Widget m ->
  WidgetEnvT m (Widget m)
make transaction size actions widget = do
  branchNameEdits <- traverse makeBranchNameEdit $ branches actions
  branchSelector <-
    BWidgets.makeChoiceWidget (setCurrentBranch actions)
    branchNameEdits (currentBranch actions)
    branchSelectionFocusDelegatorConfig
    Config.selectedBranchColor Box.vertical
    WidgetIds.branchSelection
  return .
    Widget.strongerEvents (globalEventMap actions) $
    Edges.makeVertical size widget branchSelector
  where
    makeBranchNameEdit branch = do
      let branchEditId = WidgetIds.fromGuid $ Branch.guid branch
      nameProp <-
        lift . transaction . (fmap . Lens.over (Property.pSet . Lens.mapped)) transaction $
        branchNameProp branch ^. Transaction.mkProperty
      branchNameEdit <-
        BWidgets.wrapDelegatedOT branchNameFDConfig
        FocusDelegator.NotDelegating id
        (BWidgets.makeLineEdit nameProp)
        branchEditId
      let
        delEventMap
          | ListUtils.isLengthAtLeast 2 (branches actions) =
            Widget.keysEventMapMovesCursor
            Config.delBranchKeys (E.Doc ["Branches", "Delete"])
            (WidgetIds.fromGuid . Branch.guid <$> deleteBranch actions branch)
          | otherwise = mempty
      return (branch, branchNameEdit & Widget.weakerEvents delEventMap)
